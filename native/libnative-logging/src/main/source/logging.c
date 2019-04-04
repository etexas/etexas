/**
 * Implements functions to control logging in native applications.
 *
 * @emyers
 */

// system files
#include <errno.h>   // errno
#include <stdarg.h>  // variable length arguments
#include <stdio.h>   // console and file writing
#include <stdlib.h>  // exit on program errors, malloc
#include <string.h>  // strerror, strlen
#include <time.h>    // time structures and functions

// project files
#include "logging.h"  // function implementations

/**
 * The log file path.
 */
static char *log_path_ = "/var/native.log";

/**
 * The log file.
 */
static FILE *log_file_;

/**
 * The indentation level.
 */
static int indentation_level_ = 2;

/**
 * The logging type.
 */
static LoggingType logging_type_ = CONSOLE_LOGGING;

/**
 * Returns the number of spaces for each indentation.
 *
 * @return the number of spaces for each indentation
 */
int GetIndentationLevel() {

  return indentation_level_;
}

/**
 * Sets the number of spaces for each indentation.
 *
 * @param indentation_level the number of spaces for each indentation
 */
void SetIndentationLevel(int indentation_level) {

  indentation_level_ = indentation_level;
}

/**
 * Returns a string to indent the specified number of indentations. The number
 * of spaces for each indentation is defined by the indentation level.
 *
 * @param indentations the number of indentations
 * @return a string of spaces for the number of indentations
 */
char *GetIndentationString(indentations) {

  int i;
  int space_count = indentation_level_ * indentations;
  char *indent = malloc((space_count + 1) * sizeof(char));
  for (i = 0; i < space_count; i++) {
    indent[i] = ' ';
  }

  indent[space_count] = '\0';
  return indent;
}

/**
 * Returns a string representation of the local time.
 *
 * @return a pointer to a local time string
 */
static char *GetLocalTime() {

  time_t raw_time;
  time(&raw_time);

  char *time = asctime(localtime(&raw_time));
  time[strlen(time) - 1] = 0;

  return time;
}

/**
 * Returns the logging type.
 *
 * @return the logging type
 */
LoggingType GetLoggingType() {

  return logging_type_;
}

/**
 * Sets the logging type.
 *
 * @param logging_type the logging type
 */
void SetLoggingType(LoggingType logging_type) {

  logging_type_ = logging_type;
}

/**
 * Returns a pointer to the log file path.
 *
 * @return a pointer to the log file path
 */
char *GetLogPath() {

  return log_path_;
}

/**
 * Sets the log file path.
 *
 * @param log_path a pointer to the log file path
 */
void SetLogPath(char *log_path) {

  log_path_ = log_path;
}

/**
 * Logs the given formatted string.
 *
 * @param format a pointer to the formatted string to log
 */
void Log(char *format, ...) {

  va_list args;
  va_start(args, format);

  if (logging_type_ == CONSOLE_LOGGING) {

    printf("%s: ", GetLocalTime());
    vprintf(format, args);
    fflush (stdout);

  } else if (logging_type_ == FILE_LOGGING) {

    if ((log_file_ = fopen(log_path_, "a")) == NULL) {

      perror("opening log file");

    } else if (fprintf(log_file_, "%s: ", GetLocalTime()) < 0) {

      perror("writing to log file");

    } else if (vfprintf(log_file_, format, args) < 0) {

      perror("writing to log file");
    }

    fflush(log_file_);
    if (fclose(log_file_) != 0) {
      perror("closing log file");
    }
  }
}

/**
 * Logs the given prefix string, followed by the current error number message.
 *
 * @param prefix a pointer to the prefix string to log
 */
void LogError(char *prefix) {

  Log("%s: %s\n", prefix, strerror(errno));
}

/**
 * Resets logging to begin a new log file.
 */
void ResetLogging() {

  if (remove(log_path_) != 0) {
    LogError("deleting existing log file");
  }
}
