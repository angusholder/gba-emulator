use std::env;
use std::process;

fn main() {
    let out_dir = env::var("OUT_DIR").expect("The OUT_DIR environment variable must be set");

    println!("Build script generating files in {}", out_dir);

    let cur_dir = env::current_dir().expect("Can't access current working directory");
    let crate_dir = cur_dir.as_path();

    // Make sure we rebuild if any files in src change. Cargo doesn't detect changes to our Python
    // scripts to ensure that happens here.
    println!("cargo:rerun-if-changed={}",
             crate_dir.join("src").to_string_lossy());

    let build_script = crate_dir.join("src").join("gen.py");

    let python = if cfg!(target_os = "linux") {
        "python3.6"
    } else if cfg!(target_os = "windows") {
        "python"
    } else {
        unreachable!();
    };

    let status = process::Command::new(python)
        .current_dir(crate_dir)
        .arg(build_script)
        .arg("--out-dir")
        .arg(out_dir)
        .status()
        .expect("Failed to launch second-level build script");
    if !status.success() {
        process::exit(status.code().unwrap());
    }
}
