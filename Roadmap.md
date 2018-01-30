# Roadmap #

* Detect circular/linear dependencies in _move_ type changes
* Config status marshalling
* Update (pull/fetch) all Git modules
* Conditionals: skip certain steps when a certain condition is not true
* Continuous background process (daemon?)
  * See also:
    * [Creating Launch Daemons and Agents](https://developer.apple.com/library/content/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/CreatingLaunchdJobs.html)
    * [Designing Daemons and Services](https://developer.apple.com/library/content/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/DesigningDaemons.html)
    * [How to Create a Background Service on Mac OS X](http://www.codepool.biz/how-to-create-a-background-service-on-mac-os-x.html)
    * [Adding Startup Scripts to Launch Daemon on Mac OS X Sierra 10.12.6](https://medium.com/@fahimhossain_16989/adding-startup-scripts-to-launch-daemon-on-mac-os-x-sierra-10-12-6-7e0318c74de1)
    * [What is launchd?](http://www.launchd.info/)
  * Make sure that it runs under the correct user
  * Enable automatic updates/reloads when code changes
