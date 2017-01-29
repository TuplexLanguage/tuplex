#pragma once

#include <stdarg.h>
#include <iostream>
#include <string.h>


enum Level {
    NONE,
    FATAL,
    ERROR,
    WARN,
    ALERT,
    INFO,
    NOTE,
    CONFIG,
    DEBUG,
    TRACE,
    ALL
};

extern const char* LEVEL_NAMES[];

class Logger {
protected:
    const std::string name;
    Level threshold;

    Logger(const std::string& name, Level initialThreshold);

    void emit(Level level, const char* str);

public:
    inline const std::string& get_name() const { return this->name; }

    inline Level get_threshold() const         { return this->threshold; }
    inline void set_threshold(Level threshold) { this->threshold = threshold; }

    void log(Level level, const char *format, va_list ap);

    inline void log(Level lvl, const char *fmt, ...)   { va_list a; va_start(a,fmt); this->log(lvl, fmt, a); va_end(a); }
    inline void fatal(const char *fmt, ...)   { va_list a; va_start(a,fmt); this->log(FATAL, fmt, a); va_end(a); }
    inline void error(const char *fmt, ...)   { va_list a; va_start(a,fmt); this->log(ERROR, fmt, a); va_end(a); }
    inline void warning(const char *fmt, ...) { va_list a; va_start(a,fmt); this->log(WARN,  fmt, a); va_end(a); }
    inline void alert(const char *fmt, ...)   { va_list a; va_start(a,fmt); this->log(ALERT, fmt, a); va_end(a); }
    inline void info(const char *fmt, ...)    { va_list a; va_start(a,fmt); this->log(INFO,  fmt, a); va_end(a); }
    inline void note(const char *fmt, ...)    { va_list a; va_start(a,fmt); this->log(NOTE,  fmt, a); va_end(a); }
    inline void config(const char *fmt, ...)  { va_list a; va_start(a,fmt); this->log(CONFIG,fmt, a); va_end(a); }
    inline void debug(const char *fmt, ...)   { va_list a; va_start(a,fmt); this->log(DEBUG, fmt, a); va_end(a); }
    inline void trace(const char *fmt, ...)   { va_list a; va_start(a,fmt); this->log(TRACE, fmt, a); va_end(a); }

    /** Gets the Logger instance for the specified name. */
    static Logger& get(const std::string& name);

    static Level get_global_threshold();
    static void set_global_threshold(Level threshold);

    static bool get_colors_enabled();
    static void set_colors_enabled(bool enabled);
};
