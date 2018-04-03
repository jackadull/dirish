# dirish

Synchronizes file trees of project directories across multiple machines.
Useful if you use several machines for coding, and you want to keep the same project file structure across all of them.

Your individual projects are managed by Git, but the _structure_ containing all of your local clones can be managed by `dirish`.
One or more text files with a simple syntax define the layout of your local projects (or Git clones).
Those `dirish` files can themselves be managed via Git, as part of your "meta" code project.

`dirish` makes sure that this structure is always synchronized with your local file system.
It can also list all projects that currently have local changes, and pull all projects that don't have local changes.

When calling the `dirish` `sync` command, the tool checks if your local project folder structure is according to your specification.
If not, it will figure out the differences by itself, and then clone Git projects, create folders, and move projects to differnt locations, based on necessity.

Furthermore, `dirish` can conditionally enable or disable some projects.
This can be useful if you only want `dirish` to work on certain modules if a condition is met, for example a certain host is reachable through your VPN connection.

The basic job of `dirish` can be mimicked by using Git submodules; however, those can be really impractical to work with.

More documentation will follow.
This is still in large parts a work in progress.
