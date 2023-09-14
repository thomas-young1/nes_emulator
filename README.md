# NES Emulator

### Warning: Highly experimental and still in development. It's not recommended to use this project for anything yet

Assisted by Rafael Bagmanov's [guide to writing a NES Emulator](https://bugzmanov.github.io/nes_ebook/chapter_1.html) in Rust. His article is a great starting point for people who are new to this topic and it helped in development.

## Installation

Install the Rust toolchain to get started with this project.

## Usage

Run the command `cargo run` to start the emulator. To edit which cartridge is being played, currently you must edit [the line in `main.rs`](src/main.rs) where it reads in a cartridge file.

## Features in progress:

-   [PPU registers](src/bus.rs)
-   Proper [trace function](src/trace.rs) for logging
-   Debugging assembly functions to allow [nestest.nes](cartridges/nestest.nes) to fully pass
-   Developing a stronger and more comprehensive [test suite](src/tests.rs)
