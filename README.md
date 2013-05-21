Overview
========

This plugin provides the ability for your RabbitMQ server to perform
authentication against an Kerberos KDC.

Since Kerberos is a authentication protocol and cannot do authorization
this plugin depends on another auth backend to provide authZ.
By default it uses ```rabbit_auth_backend_internal``` for this. See further
down how to use a different module for authZ.

This plugin will use your systems Kerberos settings.

NOTE: This plugin will only work on >3.x!

The plugin is BSD-licensed.

Requirements
============

You can build and install it like any other plugin (see
http://www.rabbitmq.com/plugin-development.html).

Headers for Erlang and Heimdal or MIT Kerberos are needed to build it. So on
any Debian derived distro that's ```erlang-dev``` and ```heimdal-dev``` or ```libkrb5-dev```.

Compiling
=========

The plugin works both with Heimdal and MIT Kerberos.

You need to use ```CFLAGS```and ```LDFLAGS``` when running make.
When using MIT you need to add ```-DMIT``` too for it to work.

So e.g:

Mac OS X
--------
When using Heimdal from http://www.h5l.org and erlang from [Homebrew](http://brew.sh):
```sh
CFLAGS="-I/usr/heimdal/include -I/usr/local/Cellar/erlang/R15B02/lib/erlang/usr/include/" LDFLAGS="-L/usr/heimdal/lib -lkrb5 -undefined dynamic_lookup -dynamiclib" make dist
```

Ubuntu 12.04 Precise
--------------------
When ```heimdal-dev``` and ```erlang-dev``` is installed:
```sh
CFLAGS="-I/usr/lib/erlang/usr/include/ `krb5-config --cflags`" LDFLAGS="`krb5-config --libs krb5`" make dist
```

OR for MIT Kerberos:

When ```libkrb5-dev``` and ```erlang-dev``` is installed:
```sh
CFLAGS="-I/usr/lib/erlang/usr/include/ -DMIT" LDFLAGS="`krb5-config --libs krb5`" make dist
```

Usage
=====

Enabling the plugin
-------------------

* Enable the plugin ```rabbitmq_auth_backend_kerberos```, see http://www.rabbitmq.com/plugins.html
* To make RabbitMQ use the plugin, set the value of the ```auth_backends``` configuration item
for the ```rabbit``` application to include ```rabbit_auth_backend_kerberos```.
```auth_backends``` is a list of authentication providers to try in order.

Therefore a complete RabbitMQ configuration that enables this plugin would
look like this:

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

Configuring the plugin
======================

authZ\_module
-------------

Default: ```rabbit_auth_backend_internal```

The RabbitMQ auth module which should handle the authorization.

Example configuration file
==========================

```erlang
[
  {rabbit, [{auth_backends, [rabbit_auth_backend_kerberos]}]},
  {rabbit_auth_backend_kerberos, [
    {authZ_module, rabbit_auth_backend_internal}
  ]}
].
```

Acknowledgements
================

Many thanks to:

* [rabbitmq-discuss](https://lists.rabbitmq.com/cgi-bin/mailman/listinfo/rabbitmq-discuss)
and especially to [Simon MacMullen](https://github.com/simonmacmullen) and [Tim Watson](https://github.com/hyperthunk).
* [Linus Widströmer](https://github.com/lwid) for his pedagogical and encouraging attitude and help.
* [#erlang](irc://irc.freenode.net/erlang)
* [Magnus Ahltorp](https://github.com/ahltorp) for answering my questions.

All in all, a very helpful and supportive community!
