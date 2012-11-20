Overview
========

This plugin provides the ability for your RabbitMQ server to perform
authentication against an Kerberos KDC.

Since Kerberos is a authentication protocol and cannot do authorization
this plugin depends on another auth backend to provide authZ.

By default it uses ```rabbit_auth_backend_internal``` for this. See further
down how to use a different module for authZ.

This plugin will use your systems Kerberos settings.

Requirements
============

You can build and install it like any other plugin (see
http://www.rabbitmq.com/plugin-development.html).

Enabling the plugin
===================

To enable the plugin, set the value of the "auth\_backends" configuration item
for the "rabbit" application to include "rabbitmq\_auth\_backend\_kerberos".
"auth\_backends" is a list of authentication providers to try in order.

Therefore a complete RabbitMQ configuration that enables this plugin would
look like:

```erlang
[{rabbit, [{auth_backends, [rabbitmq_auth_backend_kerberos]}]}].
```

to use only Kerberos, or:

```erlang
[{rabbit,
  [{auth_backends, [rabbitmq_auth_backend_kerberos, rabbit_auth_backend_internal]}]
 }].
```

to use Kerberos and the internal database.

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
  {rabbit, [{auth_backends, [rabbitmq_auth_backend_kerberos]}]},
  {rabbitmq_auth_backend_kerberos, [
    {authZ_module, rabbit_auth_backend_internal}
  ]}
].
```
