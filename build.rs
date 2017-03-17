use std::env;
use std::process;

fn main() {
    let out_dir = env::var("OUT_DIR").expect("The OUT_DIR environment variable must be set");

    println!("Build script generating files in {}", out_dir);

    let cur_dir = env::current_dir().expect("Can't access current working directory");
    let crate_dir = cur_dir.as_path();

    // Make sure we rebuild is this build script changes.
    // I guess that won't happen if you have non-UTF8 bytes in your path names.
    // The `build.py` script prints out its own dependencies.
    println!("cargo:rerun-if-changed={}",
             crate_dir.join("build.rs").to_string_lossy());

    // Scripts are in `$crate_dir/meta`.
    let meta_dir = crate_dir.join("src");
    let build_script = meta_dir.join("gen.py");

    // Launch build script with Python. We'll just find python in the path.
    let status = process::Command::new("python3.6")
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
