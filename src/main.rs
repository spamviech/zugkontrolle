//! Steuerung einer Model-Eisenbahn über einen raspberry pi

// Enable all warnings except
// box_pointers, non_ascii_idents, unstable_features, unused_crate_dependencies
#![warn(
    absolute_paths_not_starting_with_crate,
    elided_lifetimes_in_paths,
    explicit_outlives_requirements,
    keyword_idents,
    macro_use_extern_crate,
    meta_variable_misuse,
    missing_abi,
    missing_copy_implementations,
    missing_debug_implementations,
    missing_docs,
    noop_method_call,
    pointer_structural_match,
    rust_2021_incompatible_closure_captures,
    rust_2021_incompatible_or_patterns,
    rust_2021_prefixes_incompatible_syntax,
    rust_2021_prelude_collisions,
    single_use_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    unreachable_pub,
    unsafe_code,
    unsafe_op_in_unsafe_fn,
    unused_extern_crates,
    unused_import_braces,
    unused_lifetimes,
    unused_qualifications,
    unused_results,
    variant_size_differences
)]

use iced::{Application, Settings};
use simple_logger::SimpleLogger;
use zugkontrolle::{
    application::{fonts, icon::icon},
    args::{self, Args},
    Lego, Märklin, Zugkontrolle,
};

fn main() -> Result<(), iced::Error> {
    let args = Args::from_env();
    let verbose = args.verbose;
    let zugtyp = args.zugtyp;

    let log_level = if verbose { log::LevelFilter::Debug } else { log::LevelFilter::Warn };
    SimpleLogger::new()
        .with_level(log::LevelFilter::Error)
        .with_module_level("zugkontrolle", log_level)
        .init()
        .expect("failed to initialize error logging");

    let settings = Settings {
        window: iced::window::Settings { size: (1024, 768), icon: icon(), ..Default::default() },
        default_font: Some(&fonts::REGULAR),
        ..Settings::with_flags(args)
    };

    match zugtyp {
        args::Zugtyp::Märklin => Zugkontrolle::<Märklin>::run(settings)?,
        args::Zugtyp::Lego => Zugkontrolle::<Lego>::run(settings)?,
    }

    Ok(())
}
