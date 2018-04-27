PROJECT = rabbitmq_auth_backend_kerberos
PROJECT_DESCRIPTION = A Kerberos authN backend for RabbitMQ

DEPS = rabbit_common rabbit
TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

app-c_src: export CC +=
app-c_src: export CFLAGS +=
app-c_src: export LDFLAGS +=
app-c_src clean: export C_SRC_OUTPUT_FILE +=

include rabbitmq-components.mk
include erlang.mk
