use cfg_aliases::cfg_aliases;

fn main() {
    // Setup cfg aliases
    cfg_aliases! {
        // Raspberry Pi 2/3/4 or 0/1
        raspi: { any(target = "armv7-unknown-linux-gnueabihf", target = "arm-unknown-linux-gnueabihf") },
    }
}
