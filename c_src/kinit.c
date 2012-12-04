#include <err.h>
#include <krb5.h>
#include <stdio.h>
#include <string.h>

void secure_zero(void *s, size_t n) {
  volatile char *p = s;
  while (n--) *p++ = 0;
}

int main(int argc, char **argv) {
  krb5_error_code error;
  krb5_principal  principal;
  krb5_context    context;
  krb5_creds      creds;

  char *user = argv[1];
  char password[1024];
  char *password_ptr = password;

  if (argc < 2) {
    errx(1, "Username is missing as first argument");
  }

  // Initialize krb5 context
  if ((error = krb5_init_context(&context))) {
    return error;
  }

  memset(&creds, 0, sizeof(krb5_creds));

  // Create the principal name from user and /etc/krb5.conf
  if ((error = krb5_parse_name(context, user, &principal))) {
    goto free_context;
  }

  // Read password from STDIN
  if ((NULL == fgets(password, sizeof(password), stdin))) {
    // FIXME goto free_context? Why not?
    errx(1, "Got NULL from fgets when reading password");
  }

  // Remove trailing newline
  strsep(&password_ptr, "\n");

  // Do kinit
  if ((error = krb5_get_init_creds_password(
          context, &creds, principal, password, 0, NULL, 0, NULL, NULL))) {
    krb5_err(context, 1, error, "krb5_get_init_creds_password");
  }

  /* Zero out password securely since memset can get optimized away if compiled
   * with -01 or higher and not being used afterwards. */
  secure_zero(password, sizeof(password));

  krb5_free_cred_contents(context, &creds);
  krb5_free_principal(context, principal);

free_context:
  krb5_free_context(context);
  printf("error: %i\n", error);
  return(error);
}
