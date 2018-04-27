include env.mk

CC ?= gcc
CFLAGS += $(shell krb5-config --cflags krb5)
LDFLAGS += $(shell krb5-config --libs krb5)
CC_OPTS:= -g -pedantic -Wall -std=c99 $(CFLAGS) -fPIC -shared

all: $(C_SRC_OUTPUT_FILE)

clean:
	rm -f $(C_SRC_OUTPUT_FILE)

$(C_SRC_OUTPUT_FILE): kinit.c
	$(CC) $(CC_OPTS) -o $(C_SRC_OUTPUT_FILE) kinit.c $(LDFLAGS)
