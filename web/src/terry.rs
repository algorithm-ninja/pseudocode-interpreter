use std::collections::HashMap;

use anyhow::{bail, Context, Result};
use chrono::{DateTime, Local};
use gloo_net::http::Request;
use serde::Deserialize;
use wasm_bindgen::JsCast;
use yew::prelude::*;

use log::{info, warn};

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct TerryTaskInfo {
    pub name: String,
    pub max_score: f64,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct TerryInputInfo {
    pub id: String,
    pub path: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct TerrySubmissionInfo {
    pub score: f64,
    pub id: String,
    pub input: TerryInputInfo,
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

    for (_, task_info) in data.tasks.iter_mut() {
        // TODO(veluca): consider avoiding too many requests here.
        task_info.submissions =
            Request::get(&format!("/api/user/{user}/submissions/{}", task_info.name))
                .send()
                .await?
                .json::<TerrySubmissionList>()
                .await?
                .items
    }

    info!("{:?}", data);

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

pub async fn refresh_terry(terry: UseStateHandle<Option<TerryData>>) -> Result<()> {
    let terry_data = get_contest_info().await?;
    terry.set(Some(terry_data));
    Ok(())
}
