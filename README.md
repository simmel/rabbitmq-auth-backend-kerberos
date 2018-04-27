Overview
========

This plugin provides the ability for your RabbitMQ server authenticate users
against a Kerberos KDC using the password sent to the RabbitMQ server as part
of normal connection.

Since Kerberos is an authentication protocol and cannot do authorization this
plugin depends on the user existing with the correct tags/roles in in internal
user backend (until authzorisation has been properly fixed).

This plugin will use your systems Kerberos settings.

NOTE: This plugin will only work on >3.7.x!
For >3.3.x && <3.5.x use versions 1.x.
For >3.5.x && <3.7.x use versions 2.x.

The plugin is BSD-licensed.

Requirements
============

You can build and install it like any other plugin (see
http://www.rabbitmq.com/plugin-development.html).

Headers for Erlang and Heimdal or MIT Kerberos are needed to build it. So on
any Debian derived distro that's `erlang-dev` and `heimdal-dev` or `libkrb5-dev`.

Compiling
=========

The plugin works both with Heimdal and MIT Kerberos.

So e.g:

Mac OS X
--------
When using `heimdal` and `erlang` from [Homebrew](http://brew.sh):
```sh
LDFLAGS="-undefined dynamic_lookup -dynamiclib" make dist
```

Ubuntu 12.04 Precise
--------------------
```sh
make dist
```

Usage
=====

Enabling the plugin
-------------------

* Enable the plugin `rabbitmq_auth_backend_kerberos`, see http://www.rabbitmq.com/plugins.html
* To make RabbitMQ use the plugin, add `kerberos` to the `auth_backends` list of authentication providers to try in order.

Therefore a complete RabbitMQ configuration that enables this plugin would
look like this:

In `rabbitmq.conf`:

```ini
auth_backends.1.authn = rabbit_auth_backend_kerberos
auth_backends.1.authz = rabbit_auth_backend_internal
auth_backends.2 = rabbit_auth_backend_internal
```

or in `advanced.config`:

```erlang
[{rabbit,
  [{auth_backends, [rabbit_auth_backend_kerberos, rabbit_auth_backend_internal]}]
 }].
```

Adding the user
---------------

Now you must add the users that you want to be able to use Kerberos
authentication to the local database:

```sh
$ rabbitmqctl add_user test temporary_password
Creating user "test" ...
...done.
$ rabbitmqctl clear_password test
Clearing password for user "test" ...
...done.
```

See http://www.rabbitmq.com/management.html#permissions if you want the user to be able to access the management gui.

Acknowledgements
================

Many thanks to:

* [rabbitmq-discuss](https://lists.rabbitmq.com/cgi-bin/mailman/listinfo/rabbitmq-discuss)
and especially to [Simon MacMullen](https://github.com/simonmacmullen) and [Tim Watson](https://github.com/hyperthunk).
* [Linus Widströmer](https://github.com/lwid) for his pedagogical and encouraging attitude and help.
* [#erlang](irc://irc.freenode.net/erlang)
* [Magnus Ahltorp](https://github.com/ahltorp) for answering my questions.

All in all, a very helpful and supportive community!
