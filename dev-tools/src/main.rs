use anyhow::{Context, Result};
use std::fs;
use std::path::PathBuf;
use std::process::Command;

fn main() -> Result<()> {
    println!("Replacing rsh binary...");

    // Get the exact path where cargo install places binaries
    let binary_path = get_install_path()?;
    println!("Target binary path: {}", binary_path.display());

    // If existing binary exists, rename it with random suffix
    if binary_path.exists() {
        let random_suffix: u32 = rand::random();
        let old_binary_path = if cfg!(windows) {
            binary_path.with_file_name(format!("rsh.old.{}.exe", random_suffix))
        } else {
            binary_path.with_file_name(format!("rsh.old.{}", random_suffix))
        };

        println!(
            "Renaming existing binary to {}...",
            old_binary_path.file_name().unwrap().to_string_lossy()
        );
        fs::rename(&binary_path, &old_binary_path).with_context(|| {
            format!(
                "Failed to rename {} to {}",
                binary_path.display(),
                old_binary_path.display()
            )
        })?;
    }

    // Execute cargo install
    println!("Running cargo install --path rsh...");
    let install_result = Command::new("cargo")
        .args(["install", "--path", "rsh"])
        .status()
        .context("Failed to execute cargo install command")?;

    if !install_result.success() {
        return Err(anyhow::anyhow!("cargo install failed"));
    }

    // Clean up old binary files
    cleanup_old_binaries(&binary_path)?;

    println!("rsh binary replacement completed successfully!");
    Ok(())
}

fn get_install_path() -> Result<PathBuf> {
    // First, try to get the install root from cargo config
    let install_root = get_cargo_install_root()?;

    // Construct the binary path
    let binary_path = if cfg!(windows) {
        install_root.join("bin").join("rsh.exe")
    } else {
        install_root.join("bin").join("rsh")
    };

    Ok(binary_path)
}

fn get_cargo_install_root() -> Result<PathBuf> {
    // Method 1: Check CARGO_INSTALL_ROOT environment variable
    if let Ok(install_root) = std::env::var("CARGO_INSTALL_ROOT") {
        let path = PathBuf::from(install_root);
        if path.exists() {
            return Ok(path);
        }
    }

    // Method 2: Get from cargo config
    let config_output = Command::new("cargo")
        .args(["config", "get", "install.root"])
        .output();

    if let Ok(output) = config_output {
        if output.status.success() {
            let root_str = String::from_utf8_lossy(&output.stdout).trim().to_string();
            // Remove quotes if present
            let root_str = root_str.trim_matches('"');
            let path = PathBuf::from(root_str);
            if path.exists() {
                return Ok(path);
            }
        }
    }

    // Method 3: Check CARGO_HOME environment variable
    if let Ok(cargo_home) = std::env::var("CARGO_HOME") {
        let path = PathBuf::from(cargo_home);
        if path.join("bin").exists() {
            return Ok(path);
        }
    }

    // Method 4: Use default cargo home locations
    // Unix-like systems
    if let Ok(home) = std::env::var("HOME") {
        let cargo_home = PathBuf::from(home).join(".cargo");
        if cargo_home.join("bin").exists() {
            return Ok(cargo_home);
        }
    }

    // Windows
    if let Ok(userprofile) = std::env::var("USERPROFILE") {
        let cargo_home = PathBuf::from(userprofile).join(".cargo");
        if cargo_home.join("bin").exists() {
            return Ok(cargo_home);
        }
    }

    // Method 5: Find cargo binary and infer from its location
    let which_output = if cfg!(windows) {
        Command::new("where").arg("cargo").output()
    } else {
        Command::new("which").arg("cargo").output()
    };

    if let Ok(which_result) = which_output {
        if which_result.status.success() {
            let cargo_path = String::from_utf8_lossy(&which_result.stdout);
            let cargo_path = cargo_path.trim();
            if let Some(bin_dir) = PathBuf::from(cargo_path).parent() {
                if let Some(cargo_home) = bin_dir.parent() {
                    return Ok(cargo_home.to_path_buf());
                }
            }
        }
    }

    Err(anyhow::anyhow!("Could not determine cargo install root"))
}

fn cleanup_old_binaries(binary_path: &PathBuf) -> Result<()> {
    let bin_dir = binary_path
        .parent()
        .context("Could not get parent directory")?;

    println!("Cleaning up old binary files...");

    let entries = fs::read_dir(bin_dir)
        .with_context(|| format!("Failed to read directory {}", bin_dir.display()))?;

    for entry in entries {
        let entry = entry?;
        let file_name = entry.file_name();
        let file_name_str = file_name.to_string_lossy();

        // Match files with pattern rsh.old.*
        if file_name_str.starts_with("rsh.old.") {
            let file_path = entry.path();
            match fs::remove_file(&file_path) {
                Ok(()) => println!("Removed: {}", file_path.display()),
                Err(e) => {
                    // Running binaries cannot be deleted, but this is expected behavior
                    println!(
                        "Could not remove (possibly running): {} - {}",
                        file_path.display(),
                        e
                    );
                }
            }
        }
    }

    Ok(())
}
