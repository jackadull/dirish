# Dirish: plan

* Introduce actual parallelism within stages, see: MigrationStep2
* Fix the FlatMap optimization
* Check for deviations between config and reality
* Better test coverage
* Ensure that empty directories get removed
* IntelliJ IDEA recent project paths update
* Detect circular/linear dependencies in _move_ type changes
* Update (pull/fetch) all Git modules
* Continuous background process (daemon?)
  * See also:
    * [Creating Launch Daemons and Agents](https://developer.apple.com/library/content/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/CreatingLaunchdJobs.html)
    * [Designing Daemons and Services](https://developer.apple.com/library/content/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/DesigningDaemons.html)
    * [How to Create a Background Service on Mac OS X](http://www.codepool.biz/how-to-create-a-background-service-on-mac-os-x.html)
    * [Adding Startup Scripts to Launch Daemon on Mac OS X Sierra 10.12.6](https://medium.com/@fahimhossain_16989/adding-startup-scripts-to-launch-daemon-on-mac-os-x-sierra-10-12-6-7e0318c74de1)
    * [What is launchd?](http://www.launchd.info/)
  * Make sure that it runs under the correct user
  * Enable automatic updates/reloads when code changes
* FTP/HTTP downloads, based on GitHub releases
  * [Example](https://github.com/scala/scala/releases.atom)
  * Might also be called _materialized releases_
* Soft links on the host file system, pointing into a project
* Complimentarily to regular Git push/fetch jobs, also implement an active notification protocol
  * Clients actively push a notification when they pushed something (Git hook?)
  * Other clients receive those notifications, and then pull the changes
  * Communication protocols may include chat systems, such as Slack, Twitter or IRC
* Generalize path elements
  * Instead of throwing exceptions, make the constructors return `Either`
* Better reporting of semantic errors, containing the source position
* Enable several configs side-by-side
  * Use a config-file-wide unique ID for identification
  * Either that, or work with an include mechanism
* Nicer `toString` results, all across the board
* Some way to re-upload all the projects back to a GitLab
* Maybe use [Decline](http://ben.kirw.in/decline/) for command-line arguments
* Rename the project; maybe something akin to "homeostasis"?
* Allow comments in config files
