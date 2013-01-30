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

Requirements
============

You can build and install it like any other plugin (see
http://www.rabbitmq.com/plugin-development.html).

Enabling the plugin
===================

To enable the plugin, set the value of the ```auth_backends``` configuration item
for the ```rabbit``` application to include ```rabbit_auth_backend_kerberos```.
```auth_backends``` is a list of authentication providers to try in order.

Therefore a complete RabbitMQ configuration that enables this plugin would
look like this:

```erlang
[{rabbit,
  [{auth_backends, [rabbit_auth_backend_kerberos, rabbit_auth_backend_internal]}]
 }].
```

Configuring the plugin
======================

authZ\_module
-------

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
* [#erlang](irc://irc.freenode.net/erlang)
* [Magnus Ahltorp](https://github.com/ahltorp) for answering my questions.

All in all, a very helpful and supportive community!
