use anyhow::Result;
use gloo_storage::{LocalStorage, Storage};
use log::info;
use once_cell::sync::OnceCell;
use salsa20::cipher::{KeyIvInit, StreamCipher};
use salsa20::Salsa20;
use sha2::{Digest, Sha256};
use std::sync::Mutex;

struct FileStorage {
    token: String,
    current_task: String,
}

const PAD_A: &str = "AAAAAAAA";
const PAD_B: &str = "BBBBBBBB";

impl FileStorage {
    /// Deterministic given token and task.
    /// Returns (storage_key, encryption_key)
    fn gen_keys(&self) -> (String, String) {
        let hash = |s| {
            let mut hasher = Sha256::new();
            hasher.update(s);
            hex::encode(hasher.finalize())
        };

        let a = hash(&self.token) + PAD_A + &hash(&self.current_task);
        let b = hash(&self.token) + PAD_B + &hash(&self.current_task);

        (hash(&a), hash(&b))
    }

    fn enc(&self, enc_key: &str, source: &str) -> String {
        let bytes: Vec<u8> = enc_key.bytes().collect();
        let key: [u8; 32] = bytes[0..32].try_into().unwrap();
        let nonce: [u8; 8] = bytes[32..40].try_into().unwrap();
        let mut cipher = Salsa20::new(&key.into(), &nonce.into());

        let mut buffer: Vec<_> = source.bytes().collect();
        cipher.apply_keystream(&mut buffer);
        hex::encode(&buffer)
    }

    fn dec(&self, enc_key: &str, data: &str) -> Result<String> {
        let bytes: Vec<u8> = enc_key.bytes().collect();
        let key: [u8; 32] = bytes[0..32].try_into().unwrap();
        let nonce: [u8; 8] = bytes[32..40].try_into().unwrap();
        let mut cipher = Salsa20::new(&key.into(), &nonce.into());

        let mut buffer = hex::decode(&data)?;
        cipher.apply_keystream(&mut buffer);
        Ok(String::from_utf8(buffer)?)
    }

    fn save(&self, source: &str) {
        let (storage_key, key) = self.gen_keys();
        info!("FileStorage: {storage_key} {key}");

        let data = self.enc(&key, source);
        LocalStorage::set(storage_key, data).unwrap();
    }

    fn load(&self) -> Option<String> {
        let (storage_key, key) = self.gen_keys();
        info!("FileStorage: {storage_key} {key}");

        let data: Result<String, _> = LocalStorage::get(storage_key);
        if let Ok(data) = data {
            self.dec(&key, &data).ok()
        } else {
            None
        }
    }
}

// sigh
fn instance() -> &'static Mutex<FileStorage> {
    static INSTANCE: OnceCell<Mutex<FileStorage>> = OnceCell::new();
    INSTANCE.get_or_init(|| {
        Mutex::new(FileStorage {
            token: "".to_owned(),
            current_task: "".to_owned(),
        })
    })
}

pub fn set_current_task(current_task: &str) {
    instance().lock().unwrap().current_task = current_task.to_string();
}

pub fn set_token(token: &str) {
    instance().lock().unwrap().token = token.to_string();
}

pub fn save(source: &str) {
    instance().lock().unwrap().save(source);
}

pub fn load() -> Option<String> {
    instance().lock().unwrap().load()
}
