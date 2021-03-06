Here is, more or less, my plan:

The premise:
------------

Unix permissions are insufficient -- they are good for permissions between
users, but do not help with the issue of a user wanting to run less-trusted
code.  Android permissions are inflexible (IE all permissions are enumerated by
the OS and dictated by Google), and all-or-nothing (you can't run a program
with partial permissions), it hijacks the user/group security of Unix
(therefore killing its usefulness as a multi-user system [Can I just rant a
bit?  This was a grossly stupid decision.  Every type of computer is often used
by many users.  Even in first world countries.  What if I want to let a
stranger make a phone call but not access my email?  What if I want to let a
kid play Angry Birds but not send prank texts?  What if siblings want to keep
their settings and saved games separate on their shared tablet?  Why did
Android repeat the mistakes Windows made in the 90's?]), and its interface is
bad (you agree to everything up-front, out of context).

The solution:
-------------

- A run of a program is a combination of a "permission context" and an
  executable (this allows common programs or libraries to be re-used securely,
  without needing all programs using a library or sub-process to declare
  needing all permissions used by it).  If a function requiring permission is
  called in a context lacking that permission, an exception is raised.
- Some permissions (eg. file system, network) are standard and "come with the
  OS", other permissions can come from other programs - a program or library
  can declare new permissions to extend this base set.
- Permissions can be parameterized -- Eg. file system access won't be like
  Android's nonsense "can access everything on SD card" vs accessing nothing.
  You can have things like (permission 'file-system-read "~/some/path").
  Possibly (permission 'network "some.domain.example.com").  I think there
  should be some sort of blacklisting available to mark file system subtrees to
  be private, and denied unless the permission for that sub-tree is explicit -
  eg. (permission 'file-system-read "/home/user") gets permission to all the
  child directories, except not to "/home/user/.ssh" because it is blacklisted
  in this way.  I'm not sure quite what the interface on this should be, or how
  it should be configured yet.
- It should be configured in some format amiable both to novice computer users
  who will use menu-driven GUI stuff to configure, as well as to people who
  configure everything in a text editor and track it in git.  IE it should work
  for interactive use with GUI apps and for pre-configuration for sysadmins and
  such.
- When a permission is needed that isn't already granted, a pop-up window can
  ask a user whether the permission should be granted (eg. yes/no/yes always/no
  always) so a user will understand the context the permission is needed in --
  if the user requested network access, then the permission makes sense.  If
  the request comes unexpectedly, then users will have reason to be more
  suspicious.  A configuration option will be available for the sysadmin types
  to simply reject interactive permission escalation.
- Apps can ship with a file that gives a minimal permission context to run in.
  They would presumably live in a directory like unix's .desktop files for apps
  live in.  Users would have a directory in their home that mirrors this to be
  able to save overrides to these contexts or add more (eg when you say an app
  can always use X permission).  These default permission sets could be shown
  at install-time similar to Android -- but they should really be minimal and
  probably usually empty.
- There are some more details I've thought about (eg. that some permissions
  would offer access to services given by daemons or programs that could be run
  on-demand, and probably these permissions would also have to be declared
  similarly with files in some directory), but that are probably outside the
  scope of our proof-of-concept project.
- There is presumably some "full-user" context that will essentially get all
  permissions for a user, similar to how the root user gets full access to a
  unix system regardless of specified permissions.  In other words, running in
  this permission context would be the same as Unix is now.

The implementation plan:
------------------------

We use Racket and make a new #lang language that exports functions for wrapping
things in permission checks, wraps things like file opening in a permission
check, make sure any sub-processes or other racket programs run in the same VM
cannot escalate privileges, and generally deals with all permission stuff.
Programs written in this #lang will then be permissions-checked.  We will work
out some interface for telling a process what permission context to run in if
not the default, and how/when changing contexts/escalating is allowed (eg. a
user in a shell with full-user permissions may want to run the shell's
subprocess apps with their default permission contexts, or inheriting the
current permission context.  Or one app may launch another app, but should the
new app run in its default context or in the context of the app that launches
it?  There are probably times when both are desired.).

Granted, this implementation will only secure things that are written in this
racket dialect, but this really wants to be an OS-level system, but OS hacking
would take a lot more effort to make a proof of concept with.

