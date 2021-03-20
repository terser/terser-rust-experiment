#[macro_use]
mod macros;

mod call_terser;
mod chunk;
mod get_minified_chunks;
mod rope;
mod to_segments;
mod utils;

use call_terser::call_terser;
use std::time::Instant;
use to_segments::to_segments;

#[derive(Debug, PartialEq)]
struct Timer {
    timer_name: String,
    start_time: Instant,
}

impl Timer {
    fn new(timer_name: &str) -> Timer {
        Timer {
            timer_name: String::from(timer_name),
            start_time: Instant::now(),
        }
    }
}

impl Drop for Timer {
    fn drop(self: &mut Self) {
        println!(
            "⏱️  {}: {}ms",
            self.timer_name,
            self.start_time.elapsed().as_millis()
        )
    }
}

pub fn main() -> std::io::Result<()> {
    // Minify a big file
    let source = &std::fs::read_to_string(
        // "huge.js",
        "polyfill-mini.js",
    )?;

    let _timer = Timer::new("entire process");

    let segments = {
        let _timer = Timer::new("calculating code segments");
        to_segments(source)
    };

    let minified = {
        let _timer = Timer::new("calling terser");
        call_terser(segments)?
    };

    std::fs::write("/tmp/out.js", minified)?;

    Ok(())
}
