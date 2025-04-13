## README

A man page generator with support for the [mdoc] format build using [Skribilo].

### Idea

Apparently, writing man pages (i.e., [roff]) by hand is not particularly popular among programmers these days.
Therefore, the majority of people use an intermediate language through a man page generator, such as [scdoc] or [asciidoc].
Existing tooling in this regard outputs man pages in the [`man(7)`][man] format instead of the [`mdoc(7)`][mdoc] format.
In my view, the latter is vastly superior because it supports various semantic macros which can be used by viewers to, for example, identify command-line flags.
Unfortunately, due to the lack of support for [mdoc] in existing man page generators, the format is not as widely adopted as it should be.
This projects attempts to address that by implementing a man page generator with [mdoc] support.

Adding [mdoc] support to existing intermediate languages (e.g., the markdown-inspired [scdoc]) is challenging, as—due to lack of extensibility features in Markdown—we cannot easily add custom markup for mdoc's semantic macros.
Instead of yet again implementing our own intermediate language, this project attempts to build on an existing *extensible markup language*.
Specifically, this project builds upon [Skribilo] which is itself inspired by [Skribe] and builds upon the [Scheme] programming language.

### Contributions

Technically, in Skribilo terms, this projects makes two contributions:

1. A markup *package* which extends the Skribe language with custom *Skribe expressions* to allow using mdoc's semantics markups in Skribe documents.
2. An [Skribilo engine] which allows rendering Skribilo documents in the [mdoc] format. Such documents can use Skribilo's [standard markup][Skribilo markup] (e.g., for bold text) as well as the custom markup provided by the aforementioned mdoc-specfic markup package.

### Status

This is a proof of concept which is not directly usable in the current state.
It illustrates some of the benefits of building man page tooling on top of a extensible markup language.
However, there are some caveat in this regard which are specific to Skribilo and the Skribe language:

* Skribilo focuses largely on document generation (e.g. with HTML or LaTex) and a lot of its [standard markup][Skribilo markup] cannot be easily translated to the [mdoc] format (e.g. [figures][Skribilo figures], [images][Skribilo images], or [colors][Skribilo colors]). Therefore, only a subset of the standard markup can be supported in the [mdoc] engine, which feels unsatisfactory.
* While Skribilo supports different input formats, apart from the Scheme-based Skribe language (e.g., [Outline syntax][Skribilo outline] or [Gemtext][Skribilo gemtext]), extensible markup with custom expressions is only supported in the Skribe language. This is somewhat unfortunate as, in my view, Skribe documents are quite verbose and not as enjoyable to write as documents in Outline syntax.

A prerequisite for continued success of this project would therefore probably be making some contributions to Skribilo.
For example, implementing [pollen]-like input syntax that is easy to write (like Outline syntax) but still supports extensibility through custom expressions.

### Usage Example

Run the following commands from the repository root:

	$ export GUILE_LOAD_PATH="$(pwd)/src"
	$ skribilo -t mdoc hello.scm

### License

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see <https://www.gnu.org/licenses/>.

[mdoc]: https://man.openbsd.org/mdoc.7
[roff]: https://man.openbsd.org/roff.7
[man]: https://man.openbsd.org/man.7
[scdoc]: https://git.sr.ht/~sircmpwn/scdoc
[asciidoc]: https://asciidoc.org/
[Skribilo]: https://www.nongnu.org/skribilo
[Skribilo engine]: https://www.nongnu.org/skribilo/doc/user-14.html#engines
[Skribilo markup]: https://www.nongnu.org/skribilo/doc/user-4.html#std-markups
[Skribilo figures]: https://www.nongnu.org/skribilo/doc/user-30.html#section3424
[Skribilo images]: https://www.nongnu.org/skribilo/doc/user-31.html#section3687
[Skribilo colors]: https://www.nongnu.org/skribilo/doc/user-29.html#subsection3230
[Skribilo outline]: https://www.nongnu.org/skribilo/doc/user-3.html#outline-syntax
[Skribilo gemtext]: https://www.nongnu.org/skribilo/doc/user-3.html#gemtext-syntax
[Skribe]: https://doi.org/10.1017/S0956796805005575
[Scheme]: https://www.scheme.org/
[pollen]: https://docs.racket-lang.org/pollen/quick-tour.html#%28part._.Pollen_markup%29
