#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <sqlite3.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void fail_sqlite(sqlite3 *db, const char *operation, const char *detail)
{
  const char *message = db == NULL ? detail : sqlite3_errmsg(db);
  size_t size = strlen(operation) + strlen(message) + 32;
  char *buffer = malloc(size);
  if (buffer == NULL) {
    caml_failwith("SQLite operation failed");
  }
  snprintf(buffer, size, "SQLite %s failed: %s", operation, message);
  caml_failwith(buffer);
}

static sqlite3 *open_db(value path)
{
  sqlite3 *db = NULL;
  int rc = sqlite3_open(String_val(path), &db);
  if (rc != SQLITE_OK) {
    fail_sqlite(db, "open", String_val(path));
  }
  return db;
}

static void close_db(sqlite3 *db)
{
  int rc = sqlite3_close(db);
  if (rc != SQLITE_OK) {
    fail_sqlite(db, "close", "");
  }
}

CAMLprim value todos_sqlite_exec(value path, value sql)
{
  CAMLparam2(path, sql);
  sqlite3 *db = open_db(path);
  char *error = NULL;
  int rc = sqlite3_exec(db, String_val(sql), NULL, NULL, &error);
  if (rc != SQLITE_OK) {
    const char *message = error == NULL ? sqlite3_errmsg(db) : error;
    size_t size = strlen(message) + 1;
    char *copy = malloc(size);
    if (copy == NULL) {
      sqlite3_free(error);
      close_db(db);
      caml_failwith("SQLite exec failed");
    }
    memcpy(copy, message, size);
    sqlite3_free(error);
    close_db(db);
    caml_failwith(copy);
  }
  close_db(db);
  CAMLreturn(Val_unit);
}

CAMLprim value todos_sqlite_select_content(value path, value addr)
{
  CAMLparam2(path, addr);
  CAMLlocal2(result, content);

  sqlite3 *db = open_db(path);
  const char *sql = "select content from kvs where addr = ? limit 1;";
  sqlite3_stmt *stmt = NULL;
  int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
  if (rc != SQLITE_OK) {
    close_db(db);
    fail_sqlite(db, "prepare", sql);
  }

  rc = sqlite3_bind_int(stmt, 1, Int_val(addr));
  if (rc != SQLITE_OK) {
    sqlite3_finalize(stmt);
    close_db(db);
    fail_sqlite(db, "bind", sql);
  }

  result = Val_none;
  rc = sqlite3_step(stmt);
  if (rc == SQLITE_ROW) {
    content = caml_copy_string((const char *)sqlite3_column_text(stmt, 0));
    result = caml_alloc_some(content);
  } else if (rc != SQLITE_DONE) {
    sqlite3_finalize(stmt);
    close_db(db);
    fail_sqlite(db, "step", sql);
  }

  rc = sqlite3_finalize(stmt);
  if (rc != SQLITE_OK) {
    close_db(db);
    fail_sqlite(db, "finalize", sql);
  }
  close_db(db);
  CAMLreturn(result);
}

CAMLprim value todos_sqlite_list_addresses(value path)
{
  CAMLparam1(path);
  CAMLlocal2(result, cons);

  sqlite3 *db = open_db(path);
  const char *sql = "select addr from kvs order by addr desc;";
  sqlite3_stmt *stmt = NULL;
  int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
  if (rc != SQLITE_OK) {
    close_db(db);
    fail_sqlite(db, "prepare", sql);
  }

  result = Val_emptylist;
  while ((rc = sqlite3_step(stmt)) == SQLITE_ROW) {
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, Val_int(sqlite3_column_int(stmt, 0)));
    Store_field(cons, 1, result);
    result = cons;
  }
  if (rc != SQLITE_DONE) {
    sqlite3_finalize(stmt);
    close_db(db);
    fail_sqlite(db, "step", sql);
  }
  rc = sqlite3_finalize(stmt);
  if (rc != SQLITE_OK) {
    close_db(db);
    fail_sqlite(db, "finalize", sql);
  }
  close_db(db);
  CAMLreturn(result);
}
