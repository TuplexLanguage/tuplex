#include <cstdio>
#include <map>

#include "logging.hpp"


// hackish way to set default log levels:
struct ThresholdLevel {
    const char* name;  // logger name
    Level level;
};
static struct ThresholdLevel THRESHOLD_LEVELS[] {
        { "MAIN",           CONFIG },
        { "DRIVER",         ALL },
        { "PARSER",         ALL },
        { "SYMBOLTABLE",    ALL },
        { "LLVMGEN",        ALL },
};

static Level globalThreshold = INFO;
static bool colorsEnabled = true;

const char* LEVEL_NAMES[] {
    "NONE",
    "FATAL",
    "ERROR",
    "WARN",
    "INFO",
    "CONFG",
    "DEBUG",
    "TRACE",
    "ALL"
};

static const char* LEVEL_COLORS[] = {
        "",
        "\e[41m",
        "\e[0;91m",
        "\e[0;93m",
        "\e[0;32m",
        "\e[0;94m",
        "\e[0;95m",
        "\e[0;35m",
        ""
};
//static const char* txtblk="\e[0;30m"; // Black - Regular
//static const char* txtred="\e[0;31m"; // Red
//static const char* txtgrn="\e[0;32m"; // Green
//static const char* txtylw="\e[0;33m"; // Yellow
//static const char* txtblu="\e[0;34m"; // Blue
//static const char* txtpur="\e[0;35m"; // Purple
//static const char* txtcyn="\e[0;36m"; // Cyan
//static const char* txtwht="\e[0;37m"; // White
//static const char* bldblk="\e[1;30m"; // Black - Bold
//static const char* bldred="\e[1;31m"; // Red
//static const char* bldgrn="\e[1;32m"; // Green
//static const char* bldylw="\e[1;33m"; // Yellow
//static const char* bldblu="\e[1;34m"; // Blue
//static const char* bldpur="\e[1;35m"; // Purple
//static const char* bldcyn="\e[1;36m"; // Cyan
//static const char* bldwht="\e[1;37m"; // White
//static const char* unkblk="\e[4;30m"; // Black - Underline
//static const char* undred="\e[4;31m"; // Red
//static const char* undgrn="\e[4;32m"; // Green
//static const char* undylw="\e[4;33m"; // Yellow
//static const char* undblu="\e[4;34m"; // Blue
//static const char* undpur="\e[4;35m"; // Purple
//static const char* undcyn="\e[4;36m"; // Cyan
//static const char* undwht="\e[4;37m"; // White
//static const char* bakblk="\e[40m";   // Black - Background
//static const char* bakred="\e[41m";   // Red
//static const char* bakgrn="\e[42m";   // Green
//static const char* bakylw="\e[43m";   // Yellow
//static const char* bakblu="\e[44m";   // Blue
//static const char* bakpur="\e[45m";   // Purple
//static const char* bakcyn="\e[46m";   // Cyan
//static const char* bakwht="\e[47m";   // White
static const char* txtrst="\e[0m";    // Text Reset

static std::map<const std::string, Logger*> loggers;

Logger& Logger::get(const std::string& name) {
    if (loggers.count(name))
        return *loggers.at(name);
    else {
        Level initialThreshold = INFO;
        for (int i = 0; i < sizeof(THRESHOLD_LEVELS)/sizeof(*THRESHOLD_LEVELS); i++)
            if (name == THRESHOLD_LEVELS[i].name) {
                initialThreshold = THRESHOLD_LEVELS[i].level;
                break;
            }
        auto logger = new Logger(name, initialThreshold);
        loggers.emplace(name, logger);
        return *logger;
    }
}

Level Logger::get_global_threshold()               { return globalThreshold; }
void Logger::set_global_threshold(Level threshold) { globalThreshold = threshold; }

bool Logger::get_colors_enabled() { return colorsEnabled; }
void Logger::set_colors_enabled(bool enabled) { colorsEnabled = enabled; }

Logger::Logger(const std::string& name, Level initialThreshold) : name(name), threshold(initialThreshold) {
}


void Logger::emit(Level level, const char* str) {
    if (level <= globalThreshold) {
        if (colorsEnabled)
            fprintf(stderr, "%s%-5s %-15s %s%s\n", LEVEL_COLORS[level], LEVEL_NAMES[level], this->name.c_str(), str, txtrst);
        else
            fprintf(stderr, "%-5s %-15s %s\n", LEVEL_NAMES[level], this->name.c_str(), str);
    }
}


void Logger::log(Level level, const char *format, va_list ap) {
    if (level <= this->threshold) {
        char buf[512];
        vsnprintf(buf, 512, format, ap);
        emit(level, buf);
    }
}
