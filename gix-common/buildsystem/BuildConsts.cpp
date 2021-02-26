#include "BuildConsts.h"

const QString BuildConsts::MODULE_EXECUTABLE = "exe";
const QString BuildConsts::MODULE_DYNLOAD = "dll";
const QString BuildConsts::MODULE_DEFAULT = "";

const QString BuildConsts::BUILD_ACTION_COMPILE = "compile";
const QString BuildConsts::BUILD_ACTION_PREPROC_ESQL = "preprocess_esql";
const QString BuildConsts::BUILD_ACTION_LINK = "link";
const QString BuildConsts::BUILD_ACTION_GENERATE_SYMBOLS = "generate_symbols";
const QString BuildConsts::BUILD_ACTION_COPY = "copy";
const QString BuildConsts::BUILD_ACTION_MK_LISTING = "mk_listing";
const QString BuildConsts::BUILD_ACTION_NONE = "none";

const QString BuildConsts::TYPE_OBJ = "obj";
const QString BuildConsts::TYPE_OBJ_MAIN = "objm";
const QString BuildConsts::TYPE_LIB = "lib";
const QString BuildConsts::TYPE_CBSQL = "cbsql";
const QString BuildConsts::TYPE_CBCICS = "cbcics";
const QString BuildConsts::TYPE_COBOL = "cbl";
const QString BuildConsts::TYPE_CBLPP = "cblpp";

const QString BuildConsts::TYPE_LISTING = "lst";



const QString BuildConsts::TYPE_FINAL = "__final_target";	// Dummy target for multi-target/multi-binary projects
const QString BuildConsts::TYPE_NONE = "";

