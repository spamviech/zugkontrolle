//! Kommandozeilen-Argumente.

use std::str::FromStr;

use argh::{EarlyExit, FromArgs, TopLevelCommand};
use version::version;
use zugkontrolle::application::gleis::gleise::Modus;

#[derive(Debug)]
struct Wrapper(pub Args);
impl TopLevelCommand for Wrapper {}
impl FromArgs for Wrapper {
    fn from_args(command_name: &[&str], args: &[&str]) -> Result<Self, EarlyExit> {
        if args.contains(&"--version") {
            Err(EarlyExit { output: format!("Zugkontrolle {}", version!()), status: Ok(()) })
        } else {
            Ok(Wrapper(
                if let Some((pfad, [])) = args.split_first() {
                    if pfad.starts_with("-") {
                        Args::from_args(command_name, args)?
                    } else {
                        Args::from_args(command_name, &["--pfad", pfad])?
                    }
                } else {
                    Args::from_args(command_name, args)?
                },
            ))
        }
    }
}

#[derive(Debug, FromArgs)]
// subcommand umd direkte Verwendung (impl TopLevelCommand) von `argh::from_env` zu verhindern.
#[argh(subcommand, name = "Args")]
/// Steuerung einer Modelleisenbahn über einen Raspberry Pi.
pub struct Args {
    #[argh(option, short = 'z', default = "Zugtyp::Märklin")]
    /// verwendeter Zugtyp
    pub zugtyp: Zugtyp,

    #[argh(option, short = 'p')]
    /// dateiname
    pub pfad: Option<String>,

    #[argh(option, short = 'm')]
    /// modus bei Programstart
    pub modus: Option<Modus>,

    #[argh(switch)]
    /// zeige die aktuelle Version
    version: bool,
}

impl Args {
    /// Parse Kommandozeilen-Argumente durch das `argh`-crate.
    ///
    /// Diese Methode berücksichtigt ein `--version` flag, dass zu einem `EarlyExit` führt.
    /// Ein einzelnes Argument (das nicht mit "-" beginnt) wird als Pfad interpretiert.
    pub fn from_env() -> Self {
        let Wrapper(args) = argh::from_env();
        args
    }
}

#[derive(Debug)]
pub enum Zugtyp {
    Märklin,
    Lego,
}

impl FromStr for Zugtyp {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Märklin" => Ok(Zugtyp::Märklin),
            "Lego" => Ok(Zugtyp::Lego),
            _ => Err(s.to_string()),
        }
    }
}
