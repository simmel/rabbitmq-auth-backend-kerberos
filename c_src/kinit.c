// FIXME For strsep or use strtok
#define _BSD_SOURCE

#include <krb5.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

// MIT is broken
#ifdef MIT
#define krb5_get_error_message(context,error) error_message(error)
#endif

void secure_zero(void *s, size_t n) {
  volatile char *p = s;
  while (n--) *p++ = 0;
}

struct kebab {
  krb5_error_code error;
  krb5_principal  principal;
  krb5_context    context;
  krb5_creds      creds;
  char           *password;
};

void exit_with_message(struct kebab kebab, char *tag) {
  const char * msg = krb5_get_error_message(kebab.context, kebab.error);
  if (tag != NULL)
    printf("%s: %s\n", tag, msg);

  if (msg != NULL)
    krb5_free_error_message(kebab.context, msg);

  /* Zero out password securely since memset can get optimized away if compiled
   * with -01 or higher and not being used afterwards. */
  secure_zero(kebab.password, 1024);

  if (kebab.context) {
    // TODO Check if these if:s makes sense
    if (&kebab.creds)
      krb5_free_cred_contents(kebab.context, &kebab.creds);
    // TODO Check if these if:s makes sense
    if (kebab.principal)
      krb5_free_principal(kebab.context, kebab.principal);
    krb5_free_context(kebab.context);
  }
  exit(kebab.error);
}

int main(int argc, char **argv) {
  struct kebab kebab;

  char *user = argv[1];
  char password[1024];
  char *password_ptr = password;
  kebab.password = password;

  if (argc < 2 && (kebab.error = EINVAL))
    exit_with_message(kebab, "Username is missing as first argument");

  // Initialize krb5 context
  if ((kebab.error = krb5_init_context(&kebab.context)))
    exit_with_message(kebab, "krb5_init_context");

  memset(&kebab.creds, 0, sizeof(krb5_creds));

  // Create the principal name from user and /etc/krb5.conf
  if ((kebab.error = krb5_parse_name(kebab.context, user, &kebab.principal)))
    exit_with_message(kebab, "krb5_parse_name");

  // Read password from STDIN
  if ((NULL == fgets(password, sizeof(password), stdin)) && (kebab.error = EIO))
    exit_with_message(kebab, "Got NULL from fgets when reading password");

  // Remove trailing newline
  strsep(&password_ptr, "\n");

  // Do kinit
  if ((kebab.error = krb5_get_init_creds_password(
          kebab.context, &kebab.creds, kebab.principal, password, 0, NULL, 0, NULL, NULL)))
    exit_with_message(kebab, "krb5_get_init_creds_password");

  exit_with_message(kebab, NULL);
}
