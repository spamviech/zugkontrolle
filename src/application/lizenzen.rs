//! Zeige alle Lizenzen verwendeter Open-Source Bibliotheken.

use std::{
    borrow::Cow,
    collections::BTreeMap,
    fmt::{self, Debug, Formatter},
};

use iced_native::{
    event::{self, Event},
    text,
    widget::{
        button::{self, Button},
        scrollable::{self, Scrollable},
        Column, Container, Row, Rule, Space, Text,
    },
    Clipboard, Element, Layout, Length, Point, Renderer, Shell, Widget,
};

use crate::application::{
    fonts,
    macros::reexport_no_event_methods,
    style::{hintergrund, linie::TRENNLINIE},
};

#[derive(Debug, Clone)]
enum InterneNachricht {
    Aktuell(&'static str, fn() -> Cow<'static, str>),
    Schließen,
}

/// Nachricht, die von einem [Lizenzen]-Widget erzeugt wird.
#[derive(Debug, Clone, Copy)]
pub enum Nachricht {
    /// Schließe die [Lizenzen]-Anzeige.
    Schließen,
}

/// Zustand eines [Lizenzen]-Widgets.
#[derive(Debug)]
pub struct Zustand {
    lizenzen_und_button_states: BTreeMap<&'static str, (button::State, fn() -> Cow<'static, str>)>,
    scrollable_buttons: scrollable::State,
    scrollable_text: scrollable::State,
    scrollable_text_zurücksetzen: bool,
    schließen: button::State,
    aktuell: Option<(&'static str, Cow<'static, str>)>,
}

impl Zustand {
    /// Erstellen einen neuen [Zustand] eines [Lizenzen]-Widgets.
    pub fn neu(
        lizenzen: impl IntoIterator<Item = (&'static str, fn() -> Cow<'static, str>)>,
    ) -> Self {
        let mut aktuell = None;
        let lizenzen_und_button_states = lizenzen
            .into_iter()
            .map(|(name, f)| {
                if aktuell.is_none() {
                    aktuell = Some((name, f()));
                }
                (name, (button::State::new(), f))
            })
            .collect();
        Zustand {
            lizenzen_und_button_states,
            scrollable_buttons: scrollable::State::new(),
            scrollable_text: scrollable::State::new(),
            scrollable_text_zurücksetzen: false,
            schließen: button::State::new(),
            aktuell,
        }
    }

    /// Erstellen einen neuen [Zustand] eines [Lizenzen]-Widgets.
    #[inline(always)]
    pub fn neu_mit_verwendeten_lizenzen() -> Self {
        Self::neu(verwendete_lizenzen_mock())
    }
}

/// Widget zur Anzeige der Lizenzen verwendeten Open-Source Bibliotheken.
pub struct Lizenzen<'a, R> {
    container: Container<'a, InterneNachricht, R>,
    aktuell: &'a mut Option<(&'static str, Cow<'static, str>)>,
    scrollable_text_zurücksetzen: &'a mut bool,
}

impl<R> Debug for Lizenzen<'_, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Lizenzen").field("row", &"<Row>").finish()
    }
}

const PADDING: u16 = 5;

impl<'a, R: 'a + text::Renderer> Lizenzen<'a, R> {
    /// Erstelle ein neues [Lizenzen]-Widget.
    pub fn neu(
        zustand: &'a mut Zustand,
        scrollable_style: impl Into<Box<dyn scrollable::StyleSheet + 'a>>,
    ) -> Self {
        let Zustand {
            lizenzen_und_button_states,
            scrollable_buttons,
            scrollable_text,
            scrollable_text_zurücksetzen,
            schließen,
            aktuell,
        } = zustand;
        let mut buttons = Scrollable::new(scrollable_buttons)
            .width(Length::Shrink)
            .height(Length::Fill)
            .style(scrollable_style);
        let (aktuell_name, aktuell_text) =
            if let Some((name, text)) = aktuell { (Some(*name), Some(text)) } else { (None, None) };
        for (&name, (button_state, f)) in lizenzen_und_button_states {
            buttons = buttons.push({
                let button = Button::new(button_state, Text::new(name));
                if Some(name) == aktuell_name {
                    button
                } else {
                    button.on_press(InterneNachricht::Aktuell(name, *f))
                }
            });
        }
        let column = Column::new()
            .push(buttons)
            .push(
                Button::new(schließen, Text::new("Schließen"))
                    .on_press(InterneNachricht::Schließen),
            )
            .width(Length::Shrink)
            .height(Length::Fill);
        if *scrollable_text_zurücksetzen {
            *scrollable_text = scrollable::State::new();
            *scrollable_text_zurücksetzen = false;
        }
        let mut scrollable_aktuell =
            Scrollable::new(scrollable_text).width(Length::Fill).height(Length::Fill);
        if let Some(aktuell_text) = aktuell_text {
            let text_mit_horizontalem_padding = Row::new()
                .push(Space::with_width(Length::Units(PADDING)))
                .push(Text::new(aktuell_text.as_ref()).width(Length::Fill).height(Length::Shrink))
                .push(Space::with_width(Length::Units(PADDING)))
                .width(Length::Fill)
                .height(Length::Shrink);
            scrollable_aktuell = scrollable_aktuell
                .push(Space::with_height(Length::Units(PADDING)))
                .push(text_mit_horizontalem_padding)
                .push(Space::with_height(Length::Units(PADDING)))
        }
        let container = Container::new(
            Row::new()
                .push(column)
                .push(Rule::vertical(1).style(TRENNLINIE))
                .push(scrollable_aktuell),
        )
        .style(hintergrund::WEIß);
        Lizenzen { container, aktuell, scrollable_text_zurücksetzen }
    }
}

impl<'a, R: Renderer> Widget<Nachricht, R> for Lizenzen<'a, R> {
    reexport_no_event_methods! {Container<'a, InterneNachricht, R>, container, InterneNachricht, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Nachricht>,
    ) -> event::Status {
        let mut interne_nachrichten = Vec::new();
        let mut interne_shell = Shell::new(&mut interne_nachrichten);
        let event_status = self.container.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut interne_shell,
        );
        if interne_shell.are_widgets_invalid() {
            shell.invalidate_widgets()
        } else {
            interne_shell.revalidate_layout(|| shell.invalidate_layout())
        }
        for interne_nachricht in interne_nachrichten {
            match interne_nachricht {
                InterneNachricht::Aktuell(name, f) => {
                    *self.aktuell = Some((name, f()));
                    *self.scrollable_text_zurücksetzen = true;
                },
                InterneNachricht::Schließen => shell.publish(Nachricht::Schließen),
            }
        }
        event_status
    }
}

impl<'a, R: 'a + Renderer> From<Lizenzen<'a, R>> for Element<'a, Nachricht, R> {
    fn from(lizenzen: Lizenzen<'a, R>) -> Self {
        Element::new(lizenzen)
    }
}

#[inline(always)]
fn mit_plain<'t>() -> Cow<'t, str> {
    mit("MIT License\n\n", "[year]", "[full_name]")
}

fn mit<'t>(prefix: &str, year: &str, full_name: &str) -> Cow<'t, str> {
    Cow::Owned(format!(
        r#"{prefix}Copyright (c) {year} {full_name}

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE."#
    ))
}

#[inline(always)]
fn apache_2_0_plain<'t>() -> Cow<'t, str> {
    apache_2_0("[yyyy]", "[name of copyright owner]")
}

fn apache_2_0<'t>(year: &str, full_name: &str) -> Cow<'t, str> {
    Cow::Owned(format!(
        r#"        Apache License
        Version 2.0, January 2004
     http://www.apache.org/licenses/

TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION

1. Definitions.

"License" shall mean the terms and conditions for use, reproduction,
and distribution as defined by Sections 1 through 9 of this document.

"Licensor" shall mean the copyright owner or entity authorized by
the copyright owner that is granting the License.

"Legal Entity" shall mean the union of the acting entity and all
other entities that control, are controlled by, or are under common
control with that entity. For the purposes of this definition,
"control" means (i) the power, direct or indirect, to cause the
direction or management of such entity, whether by contract or
otherwise, or (ii) ownership of fifty percent (50%) or more of the
outstanding shares, or (iii) beneficial ownership of such entity.

"You" (or "Your") shall mean an individual or Legal Entity
exercising permissions granted by this License.

"Source" form shall mean the preferred form for making modifications,
including but not limited to software source code, documentation
source, and configuration files.

"Object" form shall mean any form resulting from mechanical
transformation or translation of a Source form, including but
not limited to compiled object code, generated documentation,
and conversions to other media types.

"Work" shall mean the work of authorship, whether in Source or
Object form, made available under the License, as indicated by a
copyright notice that is included in or attached to the work
(an example is provided in the Appendix below).

"Derivative Works" shall mean any work, whether in Source or Object
form, that is based on (or derived from) the Work and for which the
editorial revisions, annotations, elaborations, or other modifications
represent, as a whole, an original work of authorship. For the purposes
of this License, Derivative Works shall not include works that remain
separable from, or merely link (or bind by name) to the interfaces of,
the Work and Derivative Works thereof.

"Contribution" shall mean any work of authorship, including
the original version of the Work and any modifications or additions
to that Work or Derivative Works thereof, that is intentionally
submitted to Licensor for inclusion in the Work by the copyright owner
or by an individual or Legal Entity authorized to submit on behalf of
the copyright owner. For the purposes of this definition, "submitted"
means any form of electronic, verbal, or written communication sent
to the Licensor or its representatives, including but not limited to
communication on electronic mailing lists, source code control systems,
and issue tracking systems that are managed by, or on behalf of, the
Licensor for the purpose of discussing and improving the Work, but
excluding communication that is conspicuously marked or otherwise
designated in writing by the copyright owner as "Not a Contribution."

"Contributor" shall mean Licensor and any individual or Legal Entity
on behalf of whom a Contribution has been received by Licensor and
subsequently incorporated within the Work.

2. Grant of Copyright License. Subject to the terms and conditions of
this License, each Contributor hereby grants to You a perpetual,
worldwide, non-exclusive, no-charge, royalty-free, irrevocable
copyright license to reproduce, prepare Derivative Works of,
publicly display, publicly perform, sublicense, and distribute the
Work and such Derivative Works in Source or Object form.

3. Grant of Patent License. Subject to the terms and conditions of
this License, each Contributor hereby grants to You a perpetual,
worldwide, non-exclusive, no-charge, royalty-free, irrevocable
(except as stated in this section) patent license to make, have made,
use, offer to sell, sell, import, and otherwise transfer the Work,
where such license applies only to those patent claims licensable
by such Contributor that are necessarily infringed by their
Contribution(s) alone or by combination of their Contribution(s)
with the Work to which such Contribution(s) was submitted. If You
institute patent litigation against any entity (including a
cross-claim or counterclaim in a lawsuit) alleging that the Work
or a Contribution incorporated within the Work constitutes direct
or contributory patent infringement, then any patent licenses
granted to You under this License for that Work shall terminate
as of the date such litigation is filed.

4. Redistribution. You may reproduce and distribute copies of the
Work or Derivative Works thereof in any medium, with or without
modifications, and in Source or Object form, provided that You
meet the following conditions:

(a) You must give any other recipients of the Work or
Derivative Works a copy of this License; and

(b) You must cause any modified files to carry prominent notices
stating that You changed the files; and

(c) You must retain, in the Source form of any Derivative Works
that You distribute, all copyright, patent, trademark, and
attribution notices from the Source form of the Work,
excluding those notices that do not pertain to any part of
the Derivative Works; and

(d) If the Work includes a "NOTICE" text file as part of its
distribution, then any Derivative Works that You distribute must
include a readable copy of the attribution notices contained
within such NOTICE file, excluding those notices that do not
pertain to any part of the Derivative Works, in at least one
of the following places: within a NOTICE text file distributed
as part of the Derivative Works; within the Source form or
documentation, if provided along with the Derivative Works; or,
within a display generated by the Derivative Works, if and
wherever such third-party notices normally appear. The contents
of the NOTICE file are for informational purposes only and
do not modify the License. You may add Your own attribution
notices within Derivative Works that You distribute, alongside
or as an addendum to the NOTICE text from the Work, provided
that such additional attribution notices cannot be construed
as modifying the License.

You may add Your own copyright statement to Your modifications and
may provide additional or different license terms and conditions
for use, reproduction, or distribution of Your modifications, or
for any such Derivative Works as a whole, provided Your use,
reproduction, and distribution of the Work otherwise complies with
the conditions stated in this License.

5. Submission of Contributions. Unless You explicitly state otherwise,
any Contribution intentionally submitted for inclusion in the Work
by You to the Licensor shall be under the terms and conditions of
this License, without any additional terms or conditions.
Notwithstanding the above, nothing herein shall supersede or modify
the terms of any separate license agreement you may have executed
with Licensor regarding such Contributions.

6. Trademarks. This License does not grant permission to use the trade
names, trademarks, service marks, or product names of the Licensor,
except as required for reasonable and customary use in describing the
origin of the Work and reproducing the content of the NOTICE file.

7. Disclaimer of Warranty. Unless required by applicable law or
agreed to in writing, Licensor provides the Work (and each
Contributor provides its Contributions) on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied, including, without limitation, any warranties or conditions
of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
PARTICULAR PURPOSE. You are solely responsible for determining the
appropriateness of using or redistributing the Work and assume any
risks associated with Your exercise of permissions under this License.

8. Limitation of Liability. In no event and under no legal theory,
whether in tort (including negligence), contract, or otherwise,
unless required by applicable law (such as deliberate and grossly
negligent acts) or agreed to in writing, shall any Contributor be
liable to You for damages, including any direct, indirect, special,
incidental, or consequential damages of any character arising as a
result of this License or out of the use or inability to use the
Work (including but not limited to damages for loss of goodwill,
work stoppage, computer failure or malfunction, or any and all
other commercial damages or losses), even if such Contributor
has been advised of the possibility of such damages.

9. Accepting Warranty or Additional Liability. While redistributing
the Work or Derivative Works thereof, You may choose to offer,
and charge a fee for, acceptance of support, warranty, indemnity,
or other liability obligations and/or rights consistent with this
License. However, in accepting such obligations, You may act only
on Your own behalf and on Your sole responsibility, not on behalf
of any other Contributor, and only if You agree to indemnify,
defend, and hold each Contributor harmless for any liability
incurred by, or claims asserted against, such Contributor by reason
of your accepting any such warranty or additional liability.

END OF TERMS AND CONDITIONS

APPENDIX: How to apply the Apache License to your work.

To apply the Apache License to your work, attach the following
boilerplate notice, with the fields enclosed by brackets "[]"
replaced with your own identifying information. (Don't include
the brackets!)  The text should be enclosed in the appropriate
comment syntax for the file format. We also recommend that a
file or class name and description of purpose be included on the
same "printed page" as the copyright notice for easier
identification within third-party archives.

Copyright {year} {full_name}

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"#,
    ))
}

fn bsd_3<'t>(year: &str, full_name: &str) -> Cow<'t, str> {
    Cow::Owned(format!(
        r#"Copyright (c) {year}, {full_name}
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

fn cc_0<'t>() -> Cow<'t, str> {
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

fn bsl_1_0<'t>() -> Cow<'t, str> {
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

fn isc<'t>(year: &str, full_name: &str) -> Cow<'t, str> {
    Cow::Owned(format!(
        r#"Copyright © {year}, {full_name}

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

fn mpl_2_0<'t>() -> Cow<'t, str> {
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

fn zlib<'t>(year: &str, full_name: &str) -> Cow<'t, str> {
    Cow::Owned(format!(
        r#"Copyright (c) {year} {full_name}

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

// FIXME MIT AND OFL-1.1 (1): iced_glow
// die Schriftart mit entsprechender Lizenz-Datei ist in iced_graphics enthalten,
// aber die Lizenz wird in iced_glow (und iced_wgpu) angegeben

// TODO Lizenzen anpassen, so dass passende_Lizenzen nicht mehr fehlschlägt.
// TODO abweichende Dateinamen in passende_Lizenzen eintragen.
/// Die Lizenzen der verwendeter Open-Source Bibliotheken.
pub fn verwendete_lizenzen() -> Vec<(&'static str, fn() -> Cow<'static, str>)> {
    vec![
        ("SourceSerif4-Regular", || Cow::Borrowed(fonts::LICENSE)),
        ("ab_glyph-0.2.15", || apache_2_0("2020", "Alex Butler")),
        ("ab_glyph_rasterizer-0.1.5", || apache_2_0("2020", "Alex Butler")),
        ("aho-corasick-0.7.18", || mit("The ", "2015", "Andrew Gallant")),
        ("android_glue-0.2.3", mit_plain),
        ("ansi_term-0.12.1", mit_plain),
        ("approx-0.5.1", apache_2_0_plain),
        ("arrayvec-0.5.2", mit_plain),
        ("atomic-polyfill-0.1.8", mit_plain),
        ("atty-0.2.14", mit_plain),
        ("autocfg-1.1.0", mit_plain),
        ("bare-metal-0.2.5", mit_plain),
        ("bare-metal-1.0.0", mit_plain),
        ("bincode-1.3.3", mit_plain),
        ("bitfield-0.13.2", mit_plain),
        ("bitflags-1.3.2", mit_plain),
        ("bit_field-0.10.1", mit_plain),
        ("block-0.1.6", mit_plain),
        ("bumpalo-2.6.0", mit_plain),
        ("bumpalo-3.9.1", mit_plain),
        ("bytemuck-1.9.1", mit_plain),
        ("bytemuck_derive-1.1.0", mit_plain),
        ("byteorder-1.4.3", mit_plain),
        ("calloop-0.9.3", mit_plain),
        ("cc-1.0.73", mit_plain),
        ("cfg-if-0.1.10", mit_plain),
        ("cfg-if-1.0.0", mit_plain),
        ("cfg_aliases-0.1.1", mit_plain),
        ("cgl-0.3.2", mit_plain),
        ("clipboard-win-4.4.1", bsl_1_0),
        ("clipboard_macos-0.1.0", apache_2_0_plain),
        ("clipboard_wayland-0.2.0", apache_2_0_plain),
        ("clipboard_x11-0.3.1", mit_plain),
        ("cocoa-0.24.0", mit_plain),
        ("cocoa-foundation-0.1.0", mit_plain),
        ("core-foundation-0.7.0", mit_plain),
        ("core-foundation-0.9.3", mit_plain),
        ("core-foundation-sys-0.7.0", mit_plain),
        ("core-foundation-sys-0.8.3", mit_plain),
        ("core-graphics-0.19.2", mit_plain),
        ("core-graphics-0.22.3", mit_plain),
        ("core-graphics-types-0.1.1", mit_plain),
        ("core-video-sys-0.1.4", mit_plain),
        ("cortex-m-0.7.4", mit_plain),
        ("critical-section-0.2.7", mit_plain),
        ("crossbeam-channel-0.5.4", mit_plain),
        ("crossbeam-deque-0.8.1", mit_plain),
        ("crossbeam-epoch-0.9.8", mit_plain),
        ("crossbeam-utils-0.8.8", mit_plain),
        ("cty-0.2.2", mit_plain),
        ("darling-0.13.4", mit_plain),
        ("darling_core-0.13.4", mit_plain),
        ("darling_macro-0.13.4", mit_plain),
        ("difference-2.0.0", mit_plain),
        ("dispatch-0.2.0", mit_plain),
        ("dlib-0.5.0", mit_plain),
        ("dodrio-0.2.0", mpl_2_0),
        ("downcast-rs-1.2.0", mit_plain),
        ("either-1.6.1", mit_plain),
        ("embed-resource-1.7.2", mit_plain),
        ("embedded-hal-0.2.7", mit_plain),
        ("error-code-2.3.1", bsl_1_0),
        ("euclid-0.22.7", mit_plain),
        ("flexi_logger-0.22.3", mit_plain),
        ("float_next_after-0.1.5", mit_plain),
        ("fnv-1.0.7", mit_plain),
        ("foreign-types-0.3.2", mit_plain),
        ("foreign-types-shared-0.1.1", mit_plain),
        ("form_urlencoded-1.0.1", mit_plain),
        ("futures-0.3.21", mit_plain),
        ("futures-channel-0.3.21", mit_plain),
        ("futures-core-0.3.21", mit_plain),
        ("futures-executor-0.3.21", mit_plain),
        ("futures-io-0.3.21", mit_plain),
        ("futures-macro-0.3.21", mit_plain),
        ("futures-sink-0.3.21", mit_plain),
        ("futures-task-0.3.21", mit_plain),
        ("futures-util-0.3.21", mit_plain),
        ("fxhash-0.2.1", mit_plain),
        ("gethostname-0.2.3", apache_2_0_plain),
        ("getrandom-0.2.6", mit_plain),
        ("glam-0.10.2", mit_plain),
        ("glob-0.3.0", mit_plain),
        ("glow-0.11.2", mit_plain),
        ("glow_glyph-0.5.0", mit_plain),
        ("glutin-0.28.0", apache_2_0_plain),
        ("glutin_egl_sys-0.1.5", apache_2_0_plain),
        ("glutin_emscripten_sys-0.1.1", apache_2_0_plain),
        ("glutin_gles2_sys-0.1.5", apache_2_0_plain),
        ("glutin_glx_sys-0.1.7", apache_2_0_plain),
        ("glutin_wgl_sys-0.1.5", apache_2_0_plain),
        ("glyph_brush-0.7.4", apache_2_0_plain),
        ("glyph_brush_draw_cache-0.1.5", apache_2_0_plain),
        ("glyph_brush_layout-0.2.3", apache_2_0_plain),
        ("gl_generator-0.14.0", apache_2_0_plain),
        ("hash32-0.2.1", mit_plain),
        ("heapless-0.7.10", mit_plain),
        ("heck-0.4.0", mit_plain),
        ("hermit-abi-0.1.19", mit_plain),
        ("iced-0.4.2", mit_plain),
        ("iced_aw-0.1.0", mit_plain),
        ("iced_core-0.4.0", mit_plain),
        ("iced_core-0.5.0", mit_plain),
        ("iced_futures-0.3.0", mit_plain),
        ("iced_futures-0.4.0", mit_plain),
        ("iced_glow-0.3.0", mit_plain),
        ("iced_glutin-0.3.0", mit_plain),
        ("iced_graphics-0.3.0", mit_plain),
        ("iced_native-0.5.0", mit_plain),
        ("iced_style-0.3.0", mit_plain),
        ("iced_style-0.4.0", mit_plain),
        ("iced_web-0.4.0", mit_plain),
        ("iced_winit-0.4.0", mit_plain),
        ("ident_case-1.0.1", mit_plain),
        ("idna-0.2.3", mit_plain),
        ("instant-0.1.12", || bsd_3("2019", "Sébastien Crozet")),
        ("itertools-0.10.3", mit_plain),
        ("itoa-1.0.1", mit_plain),
        ("jni-sys-0.3.0", mit_plain),
        ("js-sys-0.3.57", mit_plain),
        ("khronos_api-3.1.0", apache_2_0_plain),
        ("kommandozeilen_argumente-0.2.0", mit_plain),
        ("kommandozeilen_argumente_derive-0.2.0", mit_plain),
        ("lazy_static-1.4.0", mit_plain),
        ("libc-0.2.125", mit_plain),
        ("libloading-0.7.3", || isc("2015", "Simonas Kazlauskas")),
        ("libm-0.2.2", mit_plain),
        ("linked-hash-map-0.5.4", mit_plain),
        ("lock_api-0.4.7", mit_plain),
        ("log-0.4.17", mit_plain),
        ("longest-increasing-subsequence-0.1.0", mit_plain),
        ("lyon-0.17.10", mit_plain),
        ("lyon_algorithms-0.17.7", mit_plain),
        ("lyon_geom-0.17.6", mit_plain),
        ("lyon_path-0.17.7", mit_plain),
        ("lyon_tessellation-0.17.10", mit_plain),
        ("malloc_buf-0.0.6", mit_plain),
        ("matches-0.1.9", mit_plain),
        ("memchr-2.5.0", mit_plain),
        ("memmap2-0.3.1", mit_plain),
        ("memoffset-0.6.5", mit_plain),
        ("minimal-lexical-0.2.1", mit_plain),
        ("mio-0.8.2", mit_plain),
        ("miow-0.3.7", mit_plain),
        ("nb-0.1.3", mit_plain),
        ("nb-1.0.0", mit_plain),
        ("ndk-0.5.0", mit_plain),
        ("ndk-context-0.1.1", mit_plain),
        ("ndk-glue-0.5.2", mit_plain),
        ("ndk-macro-0.3.0", mit_plain),
        ("ndk-sys-0.2.2", mit_plain),
        ("nix-0.20.0", mit_plain),
        ("nix-0.22.3", mit_plain),
        ("nom-7.1.1", mit_plain),
        ("nonempty-0.7.0", mit_plain),
        ("ntapi-0.3.7", mit_plain),
        ("num-traits-0.2.15", mit_plain),
        ("num_cpus-1.13.1", mit_plain),
        ("num_enum-0.5.7", mit_plain),
        ("num_enum_derive-0.5.7", mit_plain),
        ("num_threads-0.1.6", mit_plain),
        ("objc-0.2.7", mit_plain),
        ("objc-foundation-0.1.1", mit_plain),
        ("objc_id-0.1.1", mit_plain),
        ("once_cell-1.10.0", mit_plain),
        ("ordered-float-3.0.0", mit_plain),
        ("osmesa-sys-0.1.2", cc_0),
        ("owned_ttf_parser-0.15.0", apache_2_0_plain),
        ("parking_lot-0.11.2", mit_plain),
        ("parking_lot-0.12.0", mit_plain),
        ("parking_lot_core-0.8.5", mit_plain),
        ("parking_lot_core-0.9.3", mit_plain),
        ("percent-encoding-2.1.0", mit_plain),
        ("pin-project-lite-0.2.9", mit_plain),
        ("pin-utils-0.1.0", mit_plain),
        ("pkg-config-0.3.25", mit_plain),
        ("ppv-lite86-0.2.16", mit_plain),
        ("proc-macro-crate-1.1.3", mit_plain),
        ("proc-macro2-1.0.38", mit_plain),
        ("quote-1.0.18", mit_plain),
        ("rand-0.8.5", mit_plain),
        ("rand_chacha-0.3.1", mit_plain),
        ("rand_core-0.6.3", mit_plain),
        ("raw-window-handle-0.3.4", mit_plain),
        ("raw-window-handle-0.4.3", mit_plain),
        ("rayon-1.5.2", mit_plain),
        ("rayon-core-1.9.2", mit_plain),
        ("redox_syscall-0.2.13", mit_plain),
        ("regex-1.5.5", mit_plain),
        ("regex-syntax-0.6.25", mit_plain),
        ("riscv-0.7.0", || isc("2019-2020", "[RISC-V team][team]")),
        ("riscv-target-0.1.2", || mit("", "2020", "Ilya Epifanov")),
        ("rppal-0.13.1", mit_plain),
        ("rstar-0.9.3", mit_plain),
        ("rustc-hash-1.1.0", mit_plain),
        ("rustc_version-0.2.3", mit_plain),
        ("rustc_version-0.4.0", mit_plain),
        ("rustversion-1.0.6", mit_plain),
        ("scoped-tls-1.0.0", mit_plain),
        ("scopeguard-1.1.0", mit_plain),
        ("semver-0.9.0", mit_plain),
        ("semver-1.0.9", mit_plain),
        ("semver-parser-0.7.0", mit_plain),
        ("serde-1.0.137", mit_plain),
        ("serde_derive-1.0.137", mit_plain),
        ("shared_library-0.1.9", mit_plain),
        ("sid-0.6.1", mit_plain),
        ("slab-0.4.6", mit_plain),
        ("slotmap-1.0.6", || zlib("2021", "Orson Peters <orsonpeters@gmail.com>")),
        ("smallvec-1.8.0", mit_plain),
        ("smithay-client-toolkit-0.15.4", mit_plain),
        ("smithay-clipboard-0.6.5", mit_plain),
        ("spin-0.9.3", mit_plain),
        ("stable_deref_trait-1.2.0", mit_plain),
        ("static_assertions-1.1.0", mit_plain),
        ("str-buf-1.0.5", bsl_1_0),
        ("strsim-0.10.0", mit_plain),
        ("syn-1.0.92", mit_plain),
        ("take_mut-0.2.2", mit_plain),
        ("thiserror-1.0.31", mit_plain),
        ("thiserror-impl-1.0.31", mit_plain),
        ("time-0.3.9", mit_plain),
        ("time-macros-0.2.4", mit_plain),
        ("tinyvec-1.6.0", mit_plain),
        ("tinyvec_macros-0.1.0", mit_plain),
        ("toml-0.5.9", mit_plain),
        ("ttf-parser-0.15.0", mit_plain),
        ("twox-hash-1.6.3", mit_plain),
        ("unicase-2.6.0", mit_plain),
        ("unicode-bidi-0.3.8", mit_plain),
        ("unicode-normalization-0.1.19", mit_plain),
        ("unicode-segmentation-1.9.0", mit_plain),
        ("unicode-xid-0.2.3", mit_plain),
        ("url-2.2.2", mit_plain),
        ("vcell-0.1.3", mit_plain),
        ("version_check-0.9.4", mit_plain),
        ("void-1.0.2", mit_plain),
        ("volatile-register-0.2.1", mit_plain),
        ("vswhom-0.1.0", mit_plain),
        ("vswhom-sys-0.1.1", mit_plain),
        ("wasi-0.10.2+wasi-snapshot-preview1", mit_plain),
        ("wasi-0.11.0+wasi-snapshot-preview1", mit_plain),
        ("wasm-bindgen-0.2.80", mit_plain),
        ("wasm-bindgen-backend-0.2.80", mit_plain),
        ("wasm-bindgen-futures-0.4.30", mit_plain),
        ("wasm-bindgen-macro-0.2.80", mit_plain),
        ("wasm-bindgen-macro-support-0.2.80", mit_plain),
        ("wasm-bindgen-shared-0.2.80", mit_plain),
        ("wasm-timer-0.2.5", mit_plain),
        ("wayland-client-0.29.4", mit_plain),
        ("wayland-commons-0.29.4", mit_plain),
        ("wayland-cursor-0.29.4", mit_plain),
        ("wayland-egl-0.29.4", mit_plain),
        ("wayland-protocols-0.29.4", mit_plain),
        ("wayland-scanner-0.29.4", mit_plain),
        ("wayland-sys-0.29.4", mit_plain),
        ("web-sys-0.3.57", mit_plain),
        ("winapi-0.3.9", mit_plain),
        ("winapi-i686-pc-windows-gnu-0.4.0", mit_plain),
        ("winapi-wsapoll-0.1.1", mit_plain),
        ("winapi-x86_64-pc-windows-gnu-0.4.0", mit_plain),
        ("windows-sys-0.36.1", mit_plain),
        ("windows_aarch64_msvc-0.36.1", mit_plain),
        ("windows_i686_gnu-0.36.1", mit_plain),
        ("windows_i686_msvc-0.36.1", mit_plain),
        ("windows_x86_64_gnu-0.36.1", mit_plain),
        ("windows_x86_64_msvc-0.36.1", mit_plain),
        ("window_clipboard-0.2.2", mit_plain),
        ("winit-0.26.1", apache_2_0_plain),
        ("winreg-0.10.1", mit_plain),
        ("x11-dl-2.19.1", mit_plain),
        ("x11rb-0.8.1", mit_plain),
        ("xcursor-0.3.4", mit_plain),
        ("xi-unicode-0.3.0", apache_2_0_plain),
        ("xml-rs-0.8.4", mit_plain),
    ]
}

fn verwendete_lizenzen_mock() -> Vec<(&'static str, fn() -> Cow<'static, str>)> {
    // FIXME verwende echte Lizenzen
    let f: fn() -> Cow<'static, str> = || {
        Cow::Borrowed("Some long license text.\n\nTherefore, it needs multiple lines!\n\nNO WARRANTIES GIVEN, PROVIDED AS IS, ect.\n\n\n\n\n\n\n\n\n\n\nSome text in the middle.\n\n\n\n\n\n\nAnother midway text.\n\n\n\n\n\n\n\nYet another debug line.\n\n\n\nHello from the deep.\n\n\n\n\nA final last line after a lot of vertical space.")
    };
    let g: fn() -> Cow<'static, str> = || {
        Cow::Borrowed("Ein andere Lizenz.\nAußerdem gibt es dabei sehr lange Texte, die ausreichen sollten um neben expliziten neuen Zeilen auch automatische Zeilenumbrüche überprüfen zu können.\n\nNO WARRANTIES GIVEN, PROVIDED AS IS, ect.")
    };
    // TODO
    vec![
        ("test", f),
        ("alternativ", g),
        ("mit", || mit("", "YYYY", "Full Name")),
        ("apache-2.0", apache_2_0_plain),
    ]
}

#[test]
fn passende_lizenzen() -> Result<(), std::collections::BTreeSet<&'static str>> {
    use difference::Changeset;
    use either::Either;

    let lizenzen = verwendete_lizenzen();
    // Lizenz-Dateien, die nicht "LICENSE" heißen.
    let lizenz_dateien = BTreeMap::from([
        ("aho-corasick-0.7.18", "LICENSE-MIT"),
        ("riscv-0.7.0", "LICENSE-README"),
        ("riscv-target-0.1.2", "LICENSE-MIT"),
    ]);

    let mut unterschiede = BTreeMap::new();
    for (name, f) in lizenzen {
        let datei = lizenz_dateien.get(name).unwrap_or(&"LICENSE");
        let verwendete_lizenz = f();
        let lizenz_pfad = format!("licenses/{name}/{datei}");
        match std::fs::read_to_string(lizenz_pfad.clone()) {
            Ok(gespeicherte_lizenz) => {
                let changeset = Changeset::new(&gespeicherte_lizenz, &verwendete_lizenz, "\n");
                if !changeset.diffs.is_empty() {
                    let _ = unterschiede.insert(name, Either::Left(changeset));
                }
            },
            Err(lese_fehler) => {
                let _ = unterschiede.insert(name, Either::Right((lizenz_pfad, lese_fehler)));
            },
        }
    }

    if unterschiede.is_empty() {
        Ok(())
    } else {
        let mut not_first = false;
        for (name, changeset_oder_fehler) in unterschiede.iter() {
            if not_first {
                eprintln!("---------------------------------");
            } else {
                not_first = true;
            }
            eprintln!("{name}");
            match changeset_oder_fehler {
                Either::Left(changeset) => {
                    eprintln!("{changeset}")
                },
                Either::Right((lizenz_pfad, lese_fehler)) => {
                    eprintln!("Fehler beim lesen der gespeicherten Lizenz \"{lizenz_pfad}\":\n{lese_fehler}")
                },
            }
        }
        Err(unterschiede.into_keys().collect())
    }
}
