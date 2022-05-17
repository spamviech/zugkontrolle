//! Funktionen zum erzeugen Verwendeter Lizenz-Texte.

use std::{
    borrow::Cow,
    fmt::{self, Display, Formatter},
};

/// Erzeuge den Lizenztext für die MIT-Lizenz mit Standardwerten.
#[inline(always)]
pub(crate) fn mit_plain<'t>() -> Cow<'t, str> {
    mit(
        "MIT License\n\n",
        vec![MITCopyright::neu(true, "[year]", "[full_name]")],
        MITZeilenumbruch::Standard,
        "",
        MITEnde { punkt: true, neue_zeile: 0 },
    )
}

/// Erzeuge den Lizenztext für die MIT-Lizenz ohne Copyright-Informationen.
#[inline(always)]
pub fn mit_ohne_copyright<'t>(zeilenumbrüche: MITZeilenumbruch) -> Cow<'t, str> {
    mit("", Vec::new(), zeilenumbrüche, "", MITEnde::standard())
}

/// Anzeige der Copyright-Informationen bei einer MIT-Lizenz.
#[derive(Debug)]
pub struct MITCopyright<'t> {
    /// Wird "(c)" nach Copyright angezeigt.
    pub c_in_klammern: bool,
    /// Das Jahr in der Copyright-Information.
    pub jahr: Option<&'t str>,
    /// Der vollständige Name in der Copyright-Information.
    pub voller_name: &'t str,
}

impl<'t> MITCopyright<'t> {
    /// Erstelle ein neues [MITCopyright] struct.
    pub fn neu(
        c_in_klammern: bool,
        jahr: impl Into<Option<&'t str>>,
        voller_name: &'t str,
    ) -> Self {
        MITCopyright { c_in_klammern, jahr: jahr.into(), voller_name }
    }
}

struct VecD<'t, T>(Vec<T>, &'t str);

impl Display for MITCopyright<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let MITCopyright { c_in_klammern, jahr, voller_name } = self;
        let c_in_klammern_str = if *c_in_klammern { " (c)" } else { "" };
        write!(f, "Copyright{c_in_klammern_str} ")?;
        if let Some(jahr) = jahr {
            write!(f, "{jahr} ")?;
        }
        write!(f, "{voller_name}")
    }
}

impl Display for VecD<'_, MITCopyright<'_>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let VecD(vec, einrückung) = self;
        if !vec.is_empty() {
            for copyright in vec {
                write!(f, "{einrückung}{copyright}\n")?;
            }
            write!(f, "\n{einrückung}")?;
        }
        Ok(())
    }
}

/// Wo sind Zeilenumbrüche im MIT-Lizenztext.
#[derive(Debug, Clone, Copy)]
pub enum MITZeilenumbruch {
    /// Zeilenumbrüche, wie sie bei den meisten crates verwendet werden.
    Standard,
    /// Zeilenumbrüche, wie sie bei winreg- und wayland-*-crates verwendet werden.
    Winreg,
    /// Zeilenumbrüche, wie sie u.a. bei x11*-crates verwendet werden.
    X11,
    /// Zeilenumbrüche, wie sie u.a. bei iced*-crates verwendet werden.
    Iced,
    /// Zeilenumbrüche, wie sie beim wasm-timer-crate verwendet werden.
    WasmTimer,
    /// Keine Zeilenumbrüche, außer den Leerzeilen.
    Keine,
}

/// Das Ende einer MIT-Lizenz.
#[derive(Debug, Clone, Copy)]
pub struct MITEnde {
    /// Beende den letzten Satz mit einem '.'.
    pub punkt: bool,
    /// Beende die Lizenz mit neuen Zeilen.
    pub neue_zeile: u8,
}

impl MITEnde {
    /// Ende des MIT-Lizenztextes mit Punkt und finaler neuer Zeile.
    pub fn standard() -> Self {
        MITEnde { punkt: true, neue_zeile: 1 }
    }
}

/// Erzeuge den Lizenztext für die MIT-Lizenz.
pub fn mit<'t>(
    präfix: &str,
    copyright: Vec<MITCopyright<'_>>,
    zeilenumbrüche: MITZeilenumbruch,
    einrückung: &str,
    ende: MITEnde,
) -> Cow<'t, str> {
    let copyright_d = VecD(copyright, einrückung);
    let neue_zeile = format!("\n{einrückung}");
    let neue_zeile_str = neue_zeile.as_str();
    let mut standard = " ";
    let mut winreg = " ";
    let mut standard_winreg = " ";
    let mut x11 = " ";
    let mut iced = " ";
    let mut wasm = " ";
    let mut iced_wasm = " ";
    let mut standard_iced_wasm = " ";
    let mut standard_winreg_iced_wasm = " ";
    let mut x11_iced_wasm = " ";
    match zeilenumbrüche {
        MITZeilenumbruch::Standard => {
            standard = neue_zeile_str;
            standard_winreg = neue_zeile_str;
            standard_iced_wasm = neue_zeile_str;
            standard_winreg_iced_wasm = neue_zeile_str;
        },
        MITZeilenumbruch::Winreg => {
            winreg = neue_zeile_str;
            standard_winreg = neue_zeile_str;
            standard_winreg_iced_wasm = neue_zeile_str;
        },
        MITZeilenumbruch::X11 => {
            x11 = neue_zeile_str;
            x11_iced_wasm = neue_zeile_str;
        },
        MITZeilenumbruch::Iced => {
            iced = neue_zeile_str;
            iced_wasm = neue_zeile_str;
            standard_iced_wasm = neue_zeile_str;
            standard_winreg_iced_wasm = neue_zeile_str;
            x11_iced_wasm = neue_zeile_str;
        },
        MITZeilenumbruch::WasmTimer => {
            wasm = neue_zeile_str;
            iced_wasm = neue_zeile_str;
            standard_iced_wasm = neue_zeile_str;
            standard_winreg_iced_wasm = neue_zeile_str;
            x11_iced_wasm = neue_zeile_str;
        },
        MITZeilenumbruch::Keine => {},
    }
    let mut string = format!("{präfix}{copyright_d}");
    macro_rules! push_string {
        ($h: expr, $($t: expr),* $(,)?) => {
            string.push_str($h);
            push_string!($($t),* ,);
        };
        ($(,)?) => {};
    }
    push_string!(
        "Permission is hereby granted, free of charge, to any",
        x11,
        "person obtaining a copy",
        standard_winreg,
        "of",
        iced_wasm,
        "this software and associated",
        x11,
        "documentation files (the \"Software\"), to deal",
        standard_winreg,
        "in",
        iced_wasm,
        "the",
        x11,
        "Software without restriction, including without",
        x11,
        "limitation the rights",
        standard_winreg,
        "to",
        iced_wasm,
        "use, copy, modify, merge,",
        x11,
        "publish, distribute, sublicense, and/or sell",
        standard_winreg,
        "copies of",
        x11_iced_wasm,
        "the Software, and to permit persons to whom the Software",
        x11,
        "is",
        standard_winreg,
        "furnished to do so,",
        iced_wasm,
        "subject to the following",
        x11,
        "conditions:\n\n",
        einrückung,
        "The above copyright notice and this permission notice",
        x11,
        "shall be included in",
        winreg,
        "all",
        standard_iced_wasm,
        "copies or substantial portions",
        x11,
        "of the Software.\n\n",
        einrückung,
        "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF",
        x11,
        "ANY KIND, EXPRESS OR",
        standard_winreg_iced_wasm,
        "IMPLIED, INCLUDING BUT NOT LIMITED",
        x11,
        "TO THE WARRANTIES OF MERCHANTABILITY,",
        standard_winreg,
        "FITNESS",
        iced_wasm,
        "FOR A",
        x11,
        "PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT",
        x11,
        "SHALL THE",
        standard_winreg,
        "AUTHORS",
        wasm,
        "OR",
        iced,
        "COPYRIGHT HOLDERS BE LIABLE FOR ANY",
        x11,
        "CLAIM, DAMAGES OR OTHER",
        standard_winreg,
        "LIABILITY,",
        wasm,
        "WHETHER",
        iced,
        "IN AN ACTION",
        x11,
        "OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,",
        standard_winreg,
        "OUT OF OR",
        x11,
        "IN",
        iced_wasm,
        "CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER",
        x11,
        "DEALINGS IN",
        winreg,
        "THE",
        standard,
        "SOFTWARE",
    );
    if ende.punkt {
        string.push_str(".");
    }
    for _ in 0..ende.neue_zeile {
        string.push_str("\n");
    }
    Cow::Owned(string)
}

/// Erzeuge den Lizenztext für die Apache-2.0-Lizenz mit Standardwerten
/// ohne Einrückung und ohne Leerzeile am Anfang.
#[inline(always)]
pub fn apache_2_0_standard_nicht_eingerückt<'t>() -> Cow<'t, str> {
    apache_2_0_nicht_eingerückt(false, ApacheCopyright::standard(), true)
}

/// Erzeuge den Lizenztext für die Apache-2.0-Lizenz.
#[inline(always)]
pub fn apache_2_0_nicht_eingerückt<'t>(
    beginn_leerzeile: bool,
    copyright: ApacheCopyright<'_>,
    ende_neue_zeile: bool,
) -> Cow<'t, str> {
    apache_2_0(beginn_leerzeile, copyright, ApacheEinrückung::nicht_eingerückt(), ende_neue_zeile)
}

/// Erzeuge den Lizenztext für die Apache-2.0-Lizenz mit Standardwerten, eingerücktem Text
/// und Leerzeile am Anfang.
#[inline(always)]
pub fn apache_2_0_standard_eingerückt<'t>() -> Cow<'t, str> {
    apache_2_0_eingerückt(true, ApacheCopyright::standard(), true)
}

/// Erzeuge den Lizenztext für die Apache-2.0-Lizenz mit eingerücktem Text.
#[inline(always)]
pub fn apache_2_0_eingerückt<'t>(
    beginn_leerzeile: bool,
    copyright: ApacheCopyright<'_>,
    ende_neue_zeile: bool,
) -> Cow<'t, str> {
    apache_2_0(beginn_leerzeile, copyright, ApacheEinrückung::eingerückt(), ende_neue_zeile)
}

/// Einrückung im Lizenztext einer Apache-2.0 Lizenz.
#[derive(Debug)]
pub struct ApacheEinrückung<'t> {
    /// Einrückung des Titels (Apache License).
    pub titel: &'t str,
    /// Einrückung der Lizenz-Version.
    pub version: &'t str,
    /// Einrückung der URL zur Apache-Lizenz-Website
    pub url: &'t str,
    /// Einrückung für Header.
    pub header: &'t str,
    /// Einrückung für Text und Sub-Header.
    pub text: &'t str,
    /// Einrückung für Text von Sub-Headern.
    pub sub_text: &'t str,
    /// Einrückung für die Url mit einer Kopie des Lizenztextes.
    pub finale_url: &'t str,
}

impl ApacheEinrückung<'_> {
    /// [ApacheEinrückung] für die nicht eingerückte Version des Lizenz-Textes.
    pub fn nicht_eingerückt() -> Self {
        ApacheEinrückung {
            titel: "        ",
            version: "        ",
            url: "     ",
            header: "",
            text: "",
            sub_text: "",
            finale_url: "",
        }
    }

    /// [ApacheEinrückung] für die eingerückte Version des Lizenz-Textes.
    pub fn eingerückt() -> Self {
        ApacheEinrückung {
            titel: "                                 ",
            version: "                           ",
            url: "                        ",
            header: "   ",
            text: "      ",
            sub_text: "          ",
            finale_url: "       ",
        }
    }
}

/// Anzeige der Copyright-Informationen bei einer Apache-2.0-Lizenz.
#[derive(Debug)]
pub struct ApacheCopyright<'t> {
    /// Die verwendeten Klammern für Meta-Variablen.
    pub brackets: &'t str,
    /// Das Jahr.
    pub jahr: &'t str,
    /// Der volle Name.
    pub voller_name: &'t str,
}

impl ApacheCopyright<'_> {
    /// Standard-Werte für Copyright-Informationen in einer Apache-2.0-Lizenz.
    pub fn standard() -> Self {
        ApacheCopyright { brackets: "[]", jahr: "[yyyy]", voller_name: "[name of copyright owner]" }
    }
}

/// Erzeuge den Lizenztext für die Apache-2.0-Lizenz.
pub fn apache_2_0<'t>(
    beginn_leerzeile: bool,
    copyright: ApacheCopyright<'_>,
    einrückung: ApacheEinrückung<'_>,
    ende_neue_zeile: bool,
) -> Cow<'t, str> {
    let beginn_leerzeile_str = if beginn_leerzeile { "\n" } else { "" };
    let ApacheCopyright { brackets, jahr, voller_name } = copyright;
    let ApacheEinrückung {
        titel: indent_titel,
        version: indent_version,
        url: indent_url,
        header: indent_header,
        text: indent_text,
        sub_text: indent_sub_text,
        finale_url: indent_finale_url,
    } = einrückung;
    let ende_neue_zeile_str = if ende_neue_zeile { "\n" } else { "" };
    Cow::Owned(format!(
        r#"{beginn_leerzeile_str}{indent_titel}Apache License
{indent_version}Version 2.0, January 2004
{indent_url}http://www.apache.org/licenses/

{indent_header}TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION

{indent_header}1. Definitions.

{indent_text}"License" shall mean the terms and conditions for use, reproduction,
{indent_text}and distribution as defined by Sections 1 through 9 of this document.

{indent_text}"Licensor" shall mean the copyright owner or entity authorized by
{indent_text}the copyright owner that is granting the License.

{indent_text}"Legal Entity" shall mean the union of the acting entity and all
{indent_text}other entities that control, are controlled by, or are under common
{indent_text}control with that entity. For the purposes of this definition,
{indent_text}"control" means (i) the power, direct or indirect, to cause the
{indent_text}direction or management of such entity, whether by contract or
{indent_text}otherwise, or (ii) ownership of fifty percent (50%) or more of the
{indent_text}outstanding shares, or (iii) beneficial ownership of such entity.

{indent_text}"You" (or "Your") shall mean an individual or Legal Entity
{indent_text}exercising permissions granted by this License.

{indent_text}"Source" form shall mean the preferred form for making modifications,
{indent_text}including but not limited to software source code, documentation
{indent_text}source, and configuration files.

{indent_text}"Object" form shall mean any form resulting from mechanical
{indent_text}transformation or translation of a Source form, including but
{indent_text}not limited to compiled object code, generated documentation,
{indent_text}and conversions to other media types.

{indent_text}"Work" shall mean the work of authorship, whether in Source or
{indent_text}Object form, made available under the License, as indicated by a
{indent_text}copyright notice that is included in or attached to the work
{indent_text}(an example is provided in the Appendix below).

{indent_text}"Derivative Works" shall mean any work, whether in Source or Object
{indent_text}form, that is based on (or derived from) the Work and for which the
{indent_text}editorial revisions, annotations, elaborations, or other modifications
{indent_text}represent, as a whole, an original work of authorship. For the purposes
{indent_text}of this License, Derivative Works shall not include works that remain
{indent_text}separable from, or merely link (or bind by name) to the interfaces of,
{indent_text}the Work and Derivative Works thereof.

{indent_text}"Contribution" shall mean any work of authorship, including
{indent_text}the original version of the Work and any modifications or additions
{indent_text}to that Work or Derivative Works thereof, that is intentionally
{indent_text}submitted to Licensor for inclusion in the Work by the copyright owner
{indent_text}or by an individual or Legal Entity authorized to submit on behalf of
{indent_text}the copyright owner. For the purposes of this definition, "submitted"
{indent_text}means any form of electronic, verbal, or written communication sent
{indent_text}to the Licensor or its representatives, including but not limited to
{indent_text}communication on electronic mailing lists, source code control systems,
{indent_text}and issue tracking systems that are managed by, or on behalf of, the
{indent_text}Licensor for the purpose of discussing and improving the Work, but
{indent_text}excluding communication that is conspicuously marked or otherwise
{indent_text}designated in writing by the copyright owner as "Not a Contribution."

{indent_text}"Contributor" shall mean Licensor and any individual or Legal Entity
{indent_text}on behalf of whom a Contribution has been received by Licensor and
{indent_text}subsequently incorporated within the Work.

{indent_header}2. Grant of Copyright License. Subject to the terms and conditions of
{indent_text}this License, each Contributor hereby grants to You a perpetual,
{indent_text}worldwide, non-exclusive, no-charge, royalty-free, irrevocable
{indent_text}copyright license to reproduce, prepare Derivative Works of,
{indent_text}publicly display, publicly perform, sublicense, and distribute the
{indent_text}Work and such Derivative Works in Source or Object form.

{indent_header}3. Grant of Patent License. Subject to the terms and conditions of
{indent_text}this License, each Contributor hereby grants to You a perpetual,
{indent_text}worldwide, non-exclusive, no-charge, royalty-free, irrevocable
{indent_text}(except as stated in this section) patent license to make, have made,
{indent_text}use, offer to sell, sell, import, and otherwise transfer the Work,
{indent_text}where such license applies only to those patent claims licensable
{indent_text}by such Contributor that are necessarily infringed by their
{indent_text}Contribution(s) alone or by combination of their Contribution(s)
{indent_text}with the Work to which such Contribution(s) was submitted. If You
{indent_text}institute patent litigation against any entity (including a
{indent_text}cross-claim or counterclaim in a lawsuit) alleging that the Work
{indent_text}or a Contribution incorporated within the Work constitutes direct
{indent_text}or contributory patent infringement, then any patent licenses
{indent_text}granted to You under this License for that Work shall terminate
{indent_text}as of the date such litigation is filed.

{indent_header}4. Redistribution. You may reproduce and distribute copies of the
{indent_text}Work or Derivative Works thereof in any medium, with or without
{indent_text}modifications, and in Source or Object form, provided that You
{indent_text}meet the following conditions:

{indent_text}(a) You must give any other recipients of the Work or
{indent_sub_text}Derivative Works a copy of this License; and

{indent_text}(b) You must cause any modified files to carry prominent notices
{indent_sub_text}stating that You changed the files; and

{indent_text}(c) You must retain, in the Source form of any Derivative Works
{indent_sub_text}that You distribute, all copyright, patent, trademark, and
{indent_sub_text}attribution notices from the Source form of the Work,
{indent_sub_text}excluding those notices that do not pertain to any part of
{indent_sub_text}the Derivative Works; and

{indent_text}(d) If the Work includes a "NOTICE" text file as part of its
{indent_sub_text}distribution, then any Derivative Works that You distribute must
{indent_sub_text}include a readable copy of the attribution notices contained
{indent_sub_text}within such NOTICE file, excluding those notices that do not
{indent_sub_text}pertain to any part of the Derivative Works, in at least one
{indent_sub_text}of the following places: within a NOTICE text file distributed
{indent_sub_text}as part of the Derivative Works; within the Source form or
{indent_sub_text}documentation, if provided along with the Derivative Works; or,
{indent_sub_text}within a display generated by the Derivative Works, if and
{indent_sub_text}wherever such third-party notices normally appear. The contents
{indent_sub_text}of the NOTICE file are for informational purposes only and
{indent_sub_text}do not modify the License. You may add Your own attribution
{indent_sub_text}notices within Derivative Works that You distribute, alongside
{indent_sub_text}or as an addendum to the NOTICE text from the Work, provided
{indent_sub_text}that such additional attribution notices cannot be construed
{indent_sub_text}as modifying the License.

{indent_text}You may add Your own copyright statement to Your modifications and
{indent_text}may provide additional or different license terms and conditions
{indent_text}for use, reproduction, or distribution of Your modifications, or
{indent_text}for any such Derivative Works as a whole, provided Your use,
{indent_text}reproduction, and distribution of the Work otherwise complies with
{indent_text}the conditions stated in this License.

{indent_header}5. Submission of Contributions. Unless You explicitly state otherwise,
{indent_text}any Contribution intentionally submitted for inclusion in the Work
{indent_text}by You to the Licensor shall be under the terms and conditions of
{indent_text}this License, without any additional terms or conditions.
{indent_text}Notwithstanding the above, nothing herein shall supersede or modify
{indent_text}the terms of any separate license agreement you may have executed
{indent_text}with Licensor regarding such Contributions.

{indent_header}6. Trademarks. This License does not grant permission to use the trade
{indent_text}names, trademarks, service marks, or product names of the Licensor,
{indent_text}except as required for reasonable and customary use in describing the
{indent_text}origin of the Work and reproducing the content of the NOTICE file.

{indent_header}7. Disclaimer of Warranty. Unless required by applicable law or
{indent_text}agreed to in writing, Licensor provides the Work (and each
{indent_text}Contributor provides its Contributions) on an "AS IS" BASIS,
{indent_text}WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
{indent_text}implied, including, without limitation, any warranties or conditions
{indent_text}of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
{indent_text}PARTICULAR PURPOSE. You are solely responsible for determining the
{indent_text}appropriateness of using or redistributing the Work and assume any
{indent_text}risks associated with Your exercise of permissions under this License.

{indent_header}8. Limitation of Liability. In no event and under no legal theory,
{indent_text}whether in tort (including negligence), contract, or otherwise,
{indent_text}unless required by applicable law (such as deliberate and grossly
{indent_text}negligent acts) or agreed to in writing, shall any Contributor be
{indent_text}liable to You for damages, including any direct, indirect, special,
{indent_text}incidental, or consequential damages of any character arising as a
{indent_text}result of this License or out of the use or inability to use the
{indent_text}Work (including but not limited to damages for loss of goodwill,
{indent_text}work stoppage, computer failure or malfunction, or any and all
{indent_text}other commercial damages or losses), even if such Contributor
{indent_text}has been advised of the possibility of such damages.

{indent_header}9. Accepting Warranty or Additional Liability. While redistributing
{indent_text}the Work or Derivative Works thereof, You may choose to offer,
{indent_text}and charge a fee for, acceptance of support, warranty, indemnity,
{indent_text}or other liability obligations and/or rights consistent with this
{indent_text}License. However, in accepting such obligations, You may act only
{indent_text}on Your own behalf and on Your sole responsibility, not on behalf
{indent_text}of any other Contributor, and only if You agree to indemnify,
{indent_text}defend, and hold each Contributor harmless for any liability
{indent_text}incurred by, or claims asserted against, such Contributor by reason
{indent_text}of your accepting any such warranty or additional liability.

{indent_header}END OF TERMS AND CONDITIONS

{indent_header}APPENDIX: How to apply the Apache License to your work.

{indent_text}To apply the Apache License to your work, attach the following
{indent_text}boilerplate notice, with the fields enclosed by brackets "{brackets}"
{indent_text}replaced with your own identifying information. (Don't include
{indent_text}the brackets!)  The text should be enclosed in the appropriate
{indent_text}comment syntax for the file format. We also recommend that a
{indent_text}file or class name and description of purpose be included on the
{indent_text}same "printed page" as the copyright notice for easier
{indent_text}identification within third-party archives.

{indent_header}Copyright {jahr} {voller_name}

{indent_header}Licensed under the Apache License, Version 2.0 (the "License");
{indent_header}you may not use this file except in compliance with the License.
{indent_header}You may obtain a copy of the License at

{indent_finale_url}http://www.apache.org/licenses/LICENSE-2.0

{indent_header}Unless required by applicable law or agreed to in writing, software
{indent_header}distributed under the License is distributed on an "AS IS" BASIS,
{indent_header}WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
{indent_header}See the License for the specific language governing permissions and
{indent_header}limitations under the License.{ende_neue_zeile_str}"#,
    ))
}

/// Erzeuge den Lizenztext für die BSD-3-Lizenz.
pub fn bsd_3<'t>(jahr: &str, voller_name: &str) -> Cow<'t, str> {
    Cow::Owned(format!(
        r#"Copyright (c) {jahr}, {voller_name}
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

3. Neither the name of the author nor the names of its contributors may be used
    to endorse or promote products derived from this software without specific
    prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"#
    ))
}

/// Erzeuge den Lizenztext für die CC-0-Lizenz.
pub fn cc_0<'t>() -> Cow<'t, str> {
    Cow::Borrowed(
        r#"Statement of Purpose

The laws of most jurisdictions throughout the world automatically confer exclusive Copyright and Related Rights (defined below) upon the creator and subsequent owner(s) (each and all, an "owner") of an original work of authorship and/or a database (each, a "Work").

Certain owners wish to permanently relinquish those rights to a Work for the purpose of contributing to a commons of creative, cultural and scientific works ("Commons") that the public can reliably and without fear of later claims of infringement build upon, modify, incorporate in other works, reuse and redistribute as freely as possible in any form whatsoever and for any purposes, including without limitation commercial purposes. These owners may contribute to the Commons to promote the ideal of a free culture and the further production of creative, cultural and scientific works, or to gain reputation or greater distribution for their Work in part through the use and efforts of others.

For these and/or other purposes and motivations, and without any expectation of additional consideration or compensation, the person associating CC0 with a Work (the "Affirmer"), to the extent that he or she is an owner of Copyright and Related Rights in the Work, voluntarily elects to apply CC0 to the Work and publicly distribute the Work under its terms, with knowledge of his or her Copyright and Related Rights in the Work and the meaning and intended legal effect of CC0 on those rights.

1. Copyright and Related Rights. A Work made available under CC0 may be protected by copyright and related or neighboring rights ("Copyright and Related Rights"). Copyright and Related Rights include, but are not limited to, the following:

the right to reproduce, adapt, distribute, perform, display, communicate, and translate a Work;
moral rights retained by the original author(s) and/or performer(s);
publicity and privacy rights pertaining to a person's image or likeness depicted in a Work;
rights protecting against unfair competition in regards to a Work, subject to the limitations in paragraph 4(a), below;
rights protecting the extraction, dissemination, use and reuse of data in a Work;
database rights (such as those arising under Directive 96/9/EC of the European Parliament and of the Council of 11 March 1996 on the legal protection of databases, and under any national implementation thereof, including any amended or successor version of such directive); and
other similar, equivalent or corresponding rights throughout the world based on applicable law or treaty, and any national implementations thereof.

2. Waiver. To the greatest extent permitted by, but not in contravention of, applicable law, Affirmer hereby overtly, fully, permanently, irrevocably and unconditionally waives, abandons, and surrenders all of Affirmer's Copyright and Related Rights and associated claims and causes of action, whether now known or unknown (including existing as well as future claims and causes of action), in the Work (i) in all territories worldwide, (ii) for the maximum duration provided by applicable law or treaty (including future time extensions), (iii) in any current or future medium and for any number of copies, and (iv) for any purpose whatsoever, including without limitation commercial, advertising or promotional purposes (the "Waiver"). Affirmer makes the Waiver for the benefit of each member of the public at large and to the detriment of Affirmer's heirs and successors, fully intending that such Waiver shall not be subject to revocation, rescission, cancellation, termination, or any other legal or equitable action to disrupt the quiet enjoyment of the Work by the public as contemplated by Affirmer's express Statement of Purpose.

3. Public License Fallback. Should any part of the Waiver for any reason be judged legally invalid or ineffective under applicable law, then the Waiver shall be preserved to the maximum extent permitted taking into account Affirmer's express Statement of Purpose. In addition, to the extent the Waiver is so judged Affirmer hereby grants to each affected person a royalty-free, non transferable, non sublicensable, non exclusive, irrevocable and unconditional license to exercise Affirmer's Copyright and Related Rights in the Work (i) in all territories worldwide, (ii) for the maximum duration provided by applicable law or treaty (including future time extensions), (iii) in any current or future medium and for any number of copies, and (iv) for any purpose whatsoever, including without limitation commercial, advertising or promotional purposes (the "License"). The License shall be deemed effective as of the date CC0 was applied by Affirmer to the Work. Should any part of the License for any reason be judged legally invalid or ineffective under applicable law, such partial invalidity or ineffectiveness shall not invalidate the remainder of the License, and in such case Affirmer hereby affirms that he or she will not (i) exercise any of his or her remaining Copyright and Related Rights in the Work or (ii) assert any associated claims and causes of action with respect to the Work, in either case contrary to Affirmer's express Statement of Purpose.

4. Limitations and Disclaimers.

No trademark or patent rights held by Affirmer are waived, abandoned, surrendered, licensed or otherwise affected by this document.
Affirmer offers the Work as-is and makes no representations or warranties of any kind concerning the Work, express, implied, statutory or otherwise, including without limitation warranties of title, merchantability, fitness for a particular purpose, non infringement, or the absence of latent or other defects, accuracy, or the present or absence of errors, whether or not discoverable, all to the greatest extent permissible under applicable law.
Affirmer disclaims responsibility for clearing rights of other persons that may apply to the Work or any use thereof, including without limitation any person's Copyright and Related Rights in the Work. Further, Affirmer disclaims responsibility for obtaining any necessary consents, permissions or other rights required for any use of the Work.
Affirmer understands and acknowledges that Creative Commons is not a party to this document and has no duty or obligation with respect to this CC0 or use of the Work.
"#,
    )
}

/// Erzeuge den Lizenztext für die BSL-1.0-Lizenz.
pub fn bsl_1_0<'t>() -> Cow<'t, str> {
    Cow::Borrowed(
        r#"Boost Software License - Version 1.0 - August 17th, 2003

Permission is hereby granted, free of charge, to any person or organization
obtaining a copy of the software and accompanying documentation covered by
this license (the "Software") to use, reproduce, display, distribute,
execute, and transmit the Software, and to prepare derivative works of the
Software, and to permit third-parties to whom the Software is furnished to
do so, all subject to the following:

The copyright notices in the Software and this entire statement, including
the above license grant, this restriction and the following disclaimer,
must be included in all copies of the Software, in whole or in part, and
all derivative works of the Software, unless such copies or derivative
works are solely in the form of machine-executable object code generated by
a source language processor.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO EVENT
SHALL THE COPYRIGHT HOLDERS OR ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE
FOR ANY DAMAGES OR OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
"#,
    )
}

/// Erzeuge den Lizenztext für die ISC-Lizenz.
pub fn isc<'t>(jahr: &str, voller_name: &str) -> Cow<'t, str> {
    Cow::Owned(format!(
        r#"Copyright © {jahr}, {voller_name}

Permission to use, copy, modify, and/or distribute this software for any purpose with or without
fee is hereby granted, provided that the above copyright notice and this permission notice appear
in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
"#
    ))
}

/// Erzeuge den Lizenztext für die MPL-2.0 Lizenz.
pub fn mpl_2_0<'t>() -> Cow<'t, str> {
    Cow::Borrowed(
        r#"Mozilla Public License Version 2.0
==================================

1. Definitions
--------------

1.1. "Contributor"
    means each individual or legal entity that creates, contributes to
    the creation of, or owns Covered Software.

1.2. "Contributor Version"
    means the combination of the Contributions of others (if any) used
    by a Contributor and that particular Contributor's Contribution.

1.3. "Contribution"
    means Covered Software of a particular Contributor.

1.4. "Covered Software"
    means Source Code Form to which the initial Contributor has attached
    the notice in Exhibit A, the Executable Form of such Source Code
    Form, and Modifications of such Source Code Form, in each case
    including portions thereof.

1.5. "Incompatible With Secondary Licenses"
    means

    (a) that the initial Contributor has attached the notice described
        in Exhibit B to the Covered Software; or

    (b) that the Covered Software was made available under the terms of
        version 1.1 or earlier of the License, but not also under the
        terms of a Secondary License.

1.6. "Executable Form"
    means any form of the work other than Source Code Form.

1.7. "Larger Work"
    means a work that combines Covered Software with other material, in
    a separate file or files, that is not Covered Software.

1.8. "License"
    means this document.

1.9. "Licensable"
    means having the right to grant, to the maximum extent possible,
    whether at the time of the initial grant or subsequently, any and
    all of the rights conveyed by this License.

1.10. "Modifications"
    means any of the following:

    (a) any file in Source Code Form that results from an addition to,
        deletion from, or modification of the contents of Covered
        Software; or

    (b) any new file in Source Code Form that contains any Covered
        Software.

1.11. "Patent Claims" of a Contributor
    means any patent claim(s), including without limitation, method,
    process, and apparatus claims, in any patent Licensable by such
    Contributor that would be infringed, but for the grant of the
    License, by the making, using, selling, offering for sale, having
    made, import, or transfer of either its Contributions or its
    Contributor Version.

1.12. "Secondary License"
    means either the GNU General Public License, Version 2.0, the GNU
    Lesser General Public License, Version 2.1, the GNU Affero General
    Public License, Version 3.0, or any later versions of those
    licenses.

1.13. "Source Code Form"
    means the form of the work preferred for making modifications.

1.14. "You" (or "Your")
    means an individual or a legal entity exercising rights under this
    License. For legal entities, "You" includes any entity that
    controls, is controlled by, or is under common control with You. For
    purposes of this definition, "control" means (a) the power, direct
    or indirect, to cause the direction or management of such entity,
    whether by contract or otherwise, or (b) ownership of more than
    fifty percent (50%) of the outstanding shares or beneficial
    ownership of such entity.

2. License Grants and Conditions
--------------------------------

2.1. Grants

Each Contributor hereby grants You a world-wide, royalty-free,
non-exclusive license:

(a) under intellectual property rights (other than patent or trademark)
    Licensable by such Contributor to use, reproduce, make available,
    modify, display, perform, distribute, and otherwise exploit its
    Contributions, either on an unmodified basis, with Modifications, or
    as part of a Larger Work; and

(b) under Patent Claims of such Contributor to make, use, sell, offer
    for sale, have made, import, and otherwise transfer either its
    Contributions or its Contributor Version.

2.2. Effective Date

The licenses granted in Section 2.1 with respect to any Contribution
become effective for each Contribution on the date the Contributor first
distributes such Contribution.

2.3. Limitations on Grant Scope

The licenses granted in this Section 2 are the only rights granted under
this License. No additional rights or licenses will be implied from the
distribution or licensing of Covered Software under this License.
Notwithstanding Section 2.1(b) above, no patent license is granted by a
Contributor:

(a) for any code that a Contributor has removed from Covered Software;
    or

(b) for infringements caused by: (i) Your and any other third party's
    modifications of Covered Software, or (ii) the combination of its
    Contributions with other software (except as part of its Contributor
    Version); or

(c) under Patent Claims infringed by Covered Software in the absence of
    its Contributions.

This License does not grant any rights in the trademarks, service marks,
or logos of any Contributor (except as may be necessary to comply with
the notice requirements in Section 3.4).

2.4. Subsequent Licenses

No Contributor makes additional grants as a result of Your choice to
distribute the Covered Software under a subsequent version of this
License (see Section 10.2) or under the terms of a Secondary License (if
permitted under the terms of Section 3.3).

2.5. Representation

Each Contributor represents that the Contributor believes its
Contributions are its original creation(s) or it has sufficient rights
to grant the rights to its Contributions conveyed by this License.

2.6. Fair Use

This License is not intended to limit any rights You have under
applicable copyright doctrines of fair use, fair dealing, or other
equivalents.

2.7. Conditions

Sections 3.1, 3.2, 3.3, and 3.4 are conditions of the licenses granted
in Section 2.1.

3. Responsibilities
-------------------

3.1. Distribution of Source Form

All distribution of Covered Software in Source Code Form, including any
Modifications that You create or to which You contribute, must be under
the terms of this License. You must inform recipients that the Source
Code Form of the Covered Software is governed by the terms of this
License, and how they can obtain a copy of this License. You may not
attempt to alter or restrict the recipients' rights in the Source Code
Form.

3.2. Distribution of Executable Form

If You distribute Covered Software in Executable Form then:

(a) such Covered Software must also be made available in Source Code
    Form, as described in Section 3.1, and You must inform recipients of
    the Executable Form how they can obtain a copy of such Source Code
    Form by reasonable means in a timely manner, at a charge no more
    than the cost of distribution to the recipient; and

(b) You may distribute such Executable Form under the terms of this
    License, or sublicense it under different terms, provided that the
    license for the Executable Form does not attempt to limit or alter
    the recipients' rights in the Source Code Form under this License.

3.3. Distribution of a Larger Work

You may create and distribute a Larger Work under terms of Your choice,
provided that You also comply with the requirements of this License for
the Covered Software. If the Larger Work is a combination of Covered
Software with a work governed by one or more Secondary Licenses, and the
Covered Software is not Incompatible With Secondary Licenses, this
License permits You to additionally distribute such Covered Software
under the terms of such Secondary License(s), so that the recipient of
the Larger Work may, at their option, further distribute the Covered
Software under the terms of either this License or such Secondary
License(s).

3.4. Notices

You may not remove or alter the substance of any license notices
(including copyright notices, patent notices, disclaimers of warranty,
or limitations of liability) contained within the Source Code Form of
the Covered Software, except that You may alter any license notices to
the extent required to remedy known factual inaccuracies.

3.5. Application of Additional Terms

You may choose to offer, and to charge a fee for, warranty, support,
indemnity or liability obligations to one or more recipients of Covered
Software. However, You may do so only on Your own behalf, and not on
behalf of any Contributor. You must make it absolutely clear that any
such warranty, support, indemnity, or liability obligation is offered by
You alone, and You hereby agree to indemnify every Contributor for any
liability incurred by such Contributor as a result of warranty, support,
indemnity or liability terms You offer. You may include additional
disclaimers of warranty and limitations of liability specific to any
jurisdiction.

4. Inability to Comply Due to Statute or Regulation
---------------------------------------------------

If it is impossible for You to comply with any of the terms of this
License with respect to some or all of the Covered Software due to
statute, judicial order, or regulation then You must: (a) comply with
the terms of this License to the maximum extent possible; and (b)
describe the limitations and the code they affect. Such description must
be placed in a text file included with all distributions of the Covered
Software under this License. Except to the extent prohibited by statute
or regulation, such description must be sufficiently detailed for a
recipient of ordinary skill to be able to understand it.

5. Termination
--------------

5.1. The rights granted under this License will terminate automatically
if You fail to comply with any of its terms. However, if You become
compliant, then the rights granted under this License from a particular
Contributor are reinstated (a) provisionally, unless and until such
Contributor explicitly and finally terminates Your grants, and (b) on an
ongoing basis, if such Contributor fails to notify You of the
non-compliance by some reasonable means prior to 60 days after You have
come back into compliance. Moreover, Your grants from a particular
Contributor are reinstated on an ongoing basis if such Contributor
notifies You of the non-compliance by some reasonable means, this is the
first time You have received notice of non-compliance with this License
from such Contributor, and You become compliant prior to 30 days after
Your receipt of the notice.

5.2. If You initiate litigation against any entity by asserting a patent
infringement claim (excluding declaratory judgment actions,
counter-claims, and cross-claims) alleging that a Contributor Version
directly or indirectly infringes any patent, then the rights granted to
You by any and all Contributors for the Covered Software under Section
2.1 of this License shall terminate.

5.3. In the event of termination under Sections 5.1 or 5.2 above, all
end user license agreements (excluding distributors and resellers) which
have been validly granted by You or Your distributors under this License
prior to termination shall survive termination.

************************************************************************
*                                                                      *
*  6. Disclaimer of Warranty                                           *
*  -------------------------                                           *
*                                                                      *
*  Covered Software is provided under this License on an "as is"       *
*  basis, without warranty of any kind, either expressed, implied, or  *
*  statutory, including, without limitation, warranties that the       *
*  Covered Software is free of defects, merchantable, fit for a        *
*  particular purpose or non-infringing. The entire risk as to the     *
*  quality and performance of the Covered Software is with You.        *
*  Should any Covered Software prove defective in any respect, You     *
*  (not any Contributor) assume the cost of any necessary servicing,   *
*  repair, or correction. This disclaimer of warranty constitutes an   *
*  essential part of this License. No use of any Covered Software is   *
*  authorized under this License except under this disclaimer.         *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*  7. Limitation of Liability                                          *
*  --------------------------                                          *
*                                                                      *
*  Under no circumstances and under no legal theory, whether tort      *
*  (including negligence), contract, or otherwise, shall any           *
*  Contributor, or anyone who distributes Covered Software as          *
*  permitted above, be liable to You for any direct, indirect,         *
*  special, incidental, or consequential damages of any character      *
*  including, without limitation, damages for lost profits, loss of    *
*  goodwill, work stoppage, computer failure or malfunction, or any    *
*  and all other commercial damages or losses, even if such party      *
*  shall have been informed of the possibility of such damages. This   *
*  limitation of liability shall not apply to liability for death or   *
*  personal injury resulting from such party's negligence to the       *
*  extent applicable law prohibits such limitation. Some               *
*  jurisdictions do not allow the exclusion or limitation of           *
*  incidental or consequential damages, so this exclusion and          *
*  limitation may not apply to You.                                    *
*                                                                      *
************************************************************************

8. Litigation
-------------

Any litigation relating to this License may be brought only in the
courts of a jurisdiction where the defendant maintains its principal
place of business and such litigation shall be governed by laws of that
jurisdiction, without reference to its conflict-of-law provisions.
Nothing in this Section shall prevent a party's ability to bring
cross-claims or counter-claims.

9. Miscellaneous
----------------

This License represents the complete agreement concerning the subject
matter hereof. If any provision of this License is held to be
unenforceable, such provision shall be reformed only to the extent
necessary to make it enforceable. Any law or regulation which provides
that the language of a contract shall be construed against the drafter
shall not be used to construe this License against a Contributor.

10. Versions of the License
---------------------------

10.1. New Versions

Mozilla Foundation is the license steward. Except as provided in Section
10.3, no one other than the license steward has the right to modify or
publish new versions of this License. Each version will be given a
distinguishing version number.

10.2. Effect of New Versions

You may distribute the Covered Software under the terms of the version
of the License under which You originally received the Covered Software,
or under the terms of any subsequent version published by the license
steward.

10.3. Modified Versions

If you create software not governed by this License, and you want to
create a new license for such software, you may create and use a
modified version of this License if you rename the license and remove
any references to the name of the license steward (except to note that
such modified license differs from this License).

10.4. Distributing Source Code Form that is Incompatible With Secondary
Licenses

If You choose to distribute Source Code Form that is Incompatible With
Secondary Licenses under the terms of this version of the License, the
notice described in Exhibit B of this License must be attached.

Exhibit A - Source Code Form License Notice
-------------------------------------------

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at http://mozilla.org/MPL/2.0/.

If it is not possible or desirable to put the notice in a particular
file, then You may include the notice in a location (such as a LICENSE
file in a relevant directory) where a recipient would be likely to look
for such a notice.

You may add additional accurate notices of copyright ownership.

Exhibit B - "Incompatible With Secondary Licenses" Notice
---------------------------------------------------------

    This Source Code Form is "Incompatible With Secondary Licenses", as
    defined by the Mozilla Public License, v. 2.0.
"#,
    )
}

/// Erzeuge den Lizenztext für die ZLIB-Lizenz
pub fn zlib<'t>(jahr: &str, voller_name: &str) -> Cow<'t, str> {
    Cow::Owned(format!(
        r#"Copyright (c) {jahr} {voller_name}

This software is provided 'as-is', without any express or implied warranty. In
no event will the authors be held liable for any damages arising from the use of
this software.

Permission is granted to anyone to use this software for any purpose, including
commercial applications, and to alter it and redistribute it freely, subject to
the following restrictions:

 1. The origin of this software must not be misrepresented; you must not claim
    that you wrote the original software. If you use this software in a product,
    an acknowledgment in the product documentation would be appreciated but is
    not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
"#
    ))
}

/// Erzeuge einen Lizenz-Text für die OFL-Lizenz.
pub fn ofl_1_1<'t>(
    jahr: &str,
    voller_name: &str,
    font_name: &str,
    punkt_nach_font_name: bool,
    extra_notice: &str,
    leerzeile: bool,
    neue_zeile_vor_url: bool,
) -> Cow<'t, str> {
    let punkt_nach_font_name_str = if punkt_nach_font_name { "." } else { "" };
    let leerzeile_str = if leerzeile { "\n" } else { "" };
    let neue_zeile_vor_url_str = if neue_zeile_vor_url { "\n" } else { " " };
    Cow::Owned(format!(
        r#"Copyright {jahr} {voller_name} with Reserved Font Name {font_name}{punkt_nach_font_name_str}{extra_notice}

This Font Software is licensed under the SIL Open Font License, Version 1.1.
{leerzeile_str}This license is copied below, and is also available with a FAQ at:{neue_zeile_vor_url_str}http://scripts.sil.org/OFL


-----------------------------------------------------------
SIL OPEN FONT LICENSE Version 1.1 - 26 February 2007
-----------------------------------------------------------

PREAMBLE
The goals of the Open Font License (OFL) are to stimulate worldwide
development of collaborative font projects, to support the font creation
efforts of academic and linguistic communities, and to provide a free and
open framework in which fonts may be shared and improved in partnership
with others.

The OFL allows the licensed fonts to be used, studied, modified and
redistributed freely as long as they are not sold by themselves. The
fonts, including any derivative works, can be bundled, embedded,
redistributed and/or sold with any software provided that any reserved
names are not used by derivative works. The fonts and derivatives,
however, cannot be released under any other type of license. The
requirement for fonts to remain under this license does not apply
to any document created using the fonts or their derivatives.

DEFINITIONS
"Font Software" refers to the set of files released by the Copyright
Holder(s) under this license and clearly marked as such. This may
include source files, build scripts and documentation.

"Reserved Font Name" refers to any names specified as such after the
copyright statement(s).

"Original Version" refers to the collection of Font Software components as
distributed by the Copyright Holder(s).

"Modified Version" refers to any derivative made by adding to, deleting,
or substituting -- in part or in whole -- any of the components of the
Original Version, by changing formats or by porting the Font Software to a
new environment.

"Author" refers to any designer, engineer, programmer, technical
writer or other person who contributed to the Font Software.

PERMISSION & CONDITIONS
Permission is hereby granted, free of charge, to any person obtaining
a copy of the Font Software, to use, study, copy, merge, embed, modify,
redistribute, and sell modified and unmodified copies of the Font
Software, subject to the following conditions:

1) Neither the Font Software nor any of its individual components,
in Original or Modified Versions, may be sold by itself.

2) Original or Modified Versions of the Font Software may be bundled,
redistributed and/or sold with any software, provided that each copy
contains the above copyright notice and this license. These can be
included either as stand-alone text files, human-readable headers or
in the appropriate machine-readable metadata fields within text or
binary files as long as those fields can be easily viewed by the user.

3) No Modified Version of the Font Software may use the Reserved Font
Name(s) unless explicit written permission is granted by the corresponding
Copyright Holder. This restriction only applies to the primary font name as
presented to the users.

4) The name(s) of the Copyright Holder(s) or the Author(s) of the Font
Software shall not be used to promote, endorse or advertise any
Modified Version, except to acknowledge the contribution(s) of the
Copyright Holder(s) and the Author(s) or with their explicit written
permission.

5) The Font Software, modified or unmodified, in part or in whole,
must be distributed entirely under this license, and must not be
distributed under any other license. The requirement for fonts to
remain under this license does not apply to any document created
using the Font Software.

TERMINATION
This license becomes null and void if any of the above conditions are
not met.

DISCLAIMER
THE FONT SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT
OF COPYRIGHT, PATENT, TRADEMARK, OR OTHER RIGHT. IN NO EVENT SHALL THE
COPYRIGHT HOLDER BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
INCLUDING ANY GENERAL, SPECIAL, INDIRECT, INCIDENTAL, OR CONSEQUENTIAL
DAMAGES, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF THE USE OR INABILITY TO USE THE FONT SOFTWARE OR FROM
OTHER DEALINGS IN THE FONT SOFTWARE.
"#
    ))
}
