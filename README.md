## README

This repository provides a markup package and an engine for creating [mdoc] man pages using [Skribilo].

## Usage Example

Run the following commands from the repository root:

	$ export GUILE_LOAD_PATH="$(pwd)/src"
	$ skribilo -t mdoc hello.scm

## License

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
[Skribilo]: https://www.nongnu.org/skribilo
