/**
 * Defines the structures and functions to control logging in native
 * applications.
 *
 * @emyers
 */

#ifndef ETEXAS_NATIVE_LOGGING
#define ETEXAS_NATIVE_LOGGING

// forward type declarations
typedef enum LoggingType LoggingType;

/**
 * The logging types.
 */
enum LoggingType {

  /**
   * The console logging type indicator.
   */
  CONSOLE_LOGGING,

  /**
   * The file logging type indicator.
   */
  FILE_LOGGING
};

/**
 * Returns the number of spaces for each indentation.
 *
 * @return the number of spaces for each indentation
 */
int GetIndentationLevel();

/**
 * Sets the number of spaces for each indentation.
 *
 * @param indentation_level the number of spaces for each indentation
 */
void SetIndentationLevel(int indentation_level);

/**
 * Returns a string to indent the specified number of indentations. The number
 * of spaces for each indentation is defined by the indentation level.
 *
 * @param indentations the number of indentations
 * @return a string of spaces for the number of indentations
 */
char *GetIndentationString();

/**
 * Returns the logging type.
 *
 * @return the logging type
 */
LoggingType GetLoggingType();

/**
 * Sets the logging type.
 *
 * @param logging_type the logging type
 */
void SetLoggingType(LoggingType logging_type);

/**
 * Returns a pointer to the log file path.
 *
 * @return a pointer to the log file path
 */
char *GetLogPath();

/**
 * Sets the log file path.
 *
 * @param log_path a pointer to the log file path
 */
void SetLogPath(char *log_path);

/**
 * Logs the given formatted string.
 *
 * @param format a pointer to the formatted string to log
 */
void Log(char *format, ...);

/**
 * Logs the given prefix string, followed by the current error number message.
 *
 * @param prefix a pointer to the prefix string to log
 */
void LogError(char *prefix);

/**
 * Resets logging to begin a new log file.
 */
void ResetLogging();

#endif
