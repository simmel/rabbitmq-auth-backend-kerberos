RELEASABLE:=true
DEPS:=rabbitmq-server rabbitmq-erlang-client

C_SOURCE_DIR:=$(PACKAGE_DIR)/c_src
BINARY:=$(C_SOURCE_DIR)/kinit.so
C_SOURCE:=$(wildcard $(C_SOURCE_DIR)/*.c)

CC ?= gcc
CFLAGS ?=
LDFLAGS ?=
CC_OPTS:=-g -pedantic -Wall -std=c99 $(CFLAGS) -fPIC -shared

CONSTRUCT_APP_PREREQS:=$(BINARY)
define construct_app_commands
	mkdir -p $(APP_DIR)/priv
	cp $(BINARY) $(APP_DIR)/priv
endef

define package_rules
$(BINARY): $(C_SOURCE)
	$(CC) $(CC_OPTS) -o $$@ $(C_SOURCE) $(LDFLAGS)

$(PACKAGE_DIR)+clean::
	rm -rf $(BINARY)

endef
