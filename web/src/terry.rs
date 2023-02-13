use std::collections::HashMap;

use anyhow::{bail, Context, Error, Result};
use chrono::{DateTime, Local};
use futures::future::join_all;
use gloo_net::http::{FormData, Request};
use js_sys::Array;
use serde::Deserialize;
use wasm_bindgen::{JsCast, JsValue};
use web_sys::Blob;
use yew::prelude::*;

use log::{info, warn};

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct TerryTaskInfo {
    pub name: String,
    pub max_score: f64,
    pub statement_path: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct TerryInputInfo {
    pub id: String,
    pub path: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct TerryOutputInfo {
    pub id: String,
    pub path: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct TerrySourceInfo {
    pub id: String,
    pub path: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct TerrySubmissionInfo {
    pub score: f64,
    pub id: String,
    pub input: TerryInputInfo,
    pub source: TerrySourceInfo,
    pub output: TerryOutputInfo,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct TerrySubmissionList {
    pub items: Vec<TerrySubmissionInfo>,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct TerryUserTaskInfo {
    pub name: String,
    pub score: f64,
    #[serde(default)]
    pub max_score: f64,
    pub current_input: Option<TerryInputInfo>,
    #[serde(default)]
    pub submissions: Vec<TerrySubmissionInfo>,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct TerryContest {
    pub name: String,
    pub has_started: bool,
    pub tasks: Vec<TerryTaskInfo>,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct TerryData {
    pub token: String,
    pub name: String,
    pub surname: String,
    pub end_time: DateTime<Local>,
    pub contest: TerryContest,
    pub tasks: HashMap<String, TerryUserTaskInfo>,
}

fn document() -> web_sys::HtmlDocument {
    let document = gloo_utils::document();
    document.dyn_into::<web_sys::HtmlDocument>().unwrap()
}

fn user() -> Result<String> {
    let cookie = document().cookie().unwrap();
    let user = cookie
        .split(';')
        .map(|c| c.trim())
        .find_map(|cookie| {
            if cookie.starts_with("userToken") {
                Some(&cookie[10..])
            } else {
                None
            }
        })
        .context("no user found, please log in")?;
    Ok(user.to_owned())
}

async fn get_contest_info() -> Result<TerryData> {
    let user = user()?;
    let mut data: TerryData = Request::get(&format!("/api/user/{user}"))
        .send()
        .await?
        .json()
        .await?;

    if !data.contest.has_started {
        bail!("Contest has not started yet");
    }

    for task in data.contest.tasks.iter() {
        data.tasks
            .get_mut(&task.name)
            .context("Invalid data received from terry")?
            .max_score = task.max_score;
    }

    let mut reqs = Vec::new();
    for (_, task_info) in data.tasks.iter_mut() {
        // TODO(veluca): consider avoiding too many requests here.

        reqs.push(async {
            task_info.submissions =
                Request::get(&format!("/api/user/{user}/submissions/{}", task_info.name))
                    .send()
                    .await?
                    .json::<TerrySubmissionList>()
                    .await?
                    .items;
            Ok::<(), Error>(())
        });
    }

    for res in join_all(reqs).await {
        res?;
    }

    Ok(data)
}

#[hook]
pub fn use_terry() -> UseStateHandle<Option<TerryData>> {
    let terry = use_state(|| None);

    if terry.is_none() {
        let terry = terry.clone();
        wasm_bindgen_futures::spawn_local(async move {
            let terry_data = get_contest_info().await;
            if let Err(e) = terry_data {
                warn!("Error loading initial terry status: {e}");
                document()
                    .location()
                    .unwrap()
                    .set_href("/index.html")
                    .unwrap();
                return;
            }
            terry.set(Some(terry_data.unwrap()));
        });
    }

    terry
}

pub async fn refresh_terry(terry: UseStateHandle<TerryData>) -> Result<()> {
    let terry_data = get_contest_info().await?;
    terry.set(terry_data);
    Ok(())
}

async fn request_new_input(
    terry: UseStateHandle<TerryData>,
    task_name: &str,
) -> Result<TerryInputInfo> {
    info!("Requesting input for {task_name}");

    let data = FormData::new().unwrap();
    data.append_with_str("token", &terry.token).unwrap();
    data.append_with_str("task", task_name).unwrap();

    let info = Request::post("/api/generate_input")
        .body(data)
        .send()
        .await?
        .json::<TerryInputInfo>()
        .await?;

    refresh_terry(terry).await?;

    Ok(info)
}

pub async fn download_statement_file(
    terry: UseStateHandle<TerryData>,
    task: &str,
    name: &str,
) -> Result<String> {
    let task = terry.contest.tasks.iter().find(|x| x.name == task).unwrap();
    let path = format!(
        "/files{}",
        task.statement_path.replace("statement.md", name)
    );
    info!("Downloading {path}");
    Ok(Request::get(&path).send().await?.text().await?)
}

pub async fn download_file(path: &str) -> Result<String> {
    info!("Downloading {path}");
    Ok(Request::get(&format!("/files/{}", path))
        .send()
        .await?
        .text()
        .await?)
}

pub async fn download_input(
    terry: UseStateHandle<TerryData>,
    task_name: &str,
) -> Result<(String, TerryInputInfo)> {
    info!("Downloading input for {task_name}");
    let task_info = terry
        .tasks
        .get(task_name)
        .context("Task not found in terry data")?;

    let input_info = match &task_info.current_input {
        Some(info) => info.clone(),
        None => request_new_input(terry.clone(), task_name).await?,
    };

    let input = download_file(&input_info.path).await?;
    Ok((input, input_info))
}

async fn upload_output(task_name: &str, input_id: &str, output: &str) -> Result<TerryOutputInfo> {
    info!("Uploading output for {task_name}. Data: {output}");

    let arr = Array::new();
    arr.push(&JsValue::from_str(output));
    let blob = Blob::new_with_str_sequence(&arr).unwrap();

    let data = FormData::new().unwrap();
    data.append_with_str("input_id", input_id).unwrap();
    data.append_with_blob_and_filename("file", &blob, &format!("{task_name}_output.txt"))
        .unwrap();

    let info = Request::post("/api/upload_output")
        .body(data)
        .send()
        .await?
        .json::<TerryOutputInfo>()
        .await?;

    info!("{info:?}");

    Ok(info)
}

async fn upload_source(task_name: &str, input_id: &str, source: &str) -> Result<TerrySourceInfo> {
    info!("Uploading source for {task_name}. Data: {source}");

    let arr = Array::new();
    arr.push(&JsValue::from_str(source));
    let blob = Blob::new_with_str_sequence(&arr).unwrap();

    let data = FormData::new().unwrap();
    data.append_with_str("input_id", input_id).unwrap();
    data.append_with_blob_and_filename("file", &blob, &format!("{task_name}.srs"))
        .unwrap();

    let info = Request::post("/api/upload_source")
        .body(data)
        .send()
        .await?
        .json::<TerrySourceInfo>()
        .await?;

    info!("{info:?}");

    Ok(info)
}

pub async fn submit(
    terry: UseStateHandle<TerryData>,
    task_name: &str,
    input_id: &str,
    source: &str,
    output: &str,
) -> Result<()> {
    let source_info = upload_source(task_name, input_id, source).await?;
    let output_info = upload_output(task_name, input_id, output).await?;

    let data = FormData::new().unwrap();
    data.append_with_str("input_id", input_id).unwrap();
    data.append_with_str("source_id", &source_info.id).unwrap();
    data.append_with_str("output_id", &output_info.id).unwrap();

    let info = Request::post("/api/submit")
        .body(data)
        .send()
        .await?
        .text()
        .await?;

    info!("{info}");

    refresh_terry(terry.clone()).await?;

    Ok(())
}
