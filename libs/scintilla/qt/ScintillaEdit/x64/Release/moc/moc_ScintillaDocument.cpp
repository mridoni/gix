/****************************************************************************
** Meta object code from reading C++ file 'ScintillaDocument.h'
**
** Created by: The Qt Meta Object Compiler version 67 (Qt 5.14.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include <memory>
#include "../../../ScintillaDocument.h"
#include <QtCore/qbytearray.h>
#include <QtCore/qmetatype.h>
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'ScintillaDocument.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 67
#error "This file was generated using the moc from 5.14.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
QT_WARNING_PUSH
QT_WARNING_DISABLE_DEPRECATED
struct qt_meta_stringdata_ScintillaDocument_t {
    QByteArrayData data[19];
    char stringdata0[201];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    qptrdiff(offsetof(qt_meta_stringdata_ScintillaDocument_t, stringdata0) + ofs \
        - idx * sizeof(QByteArrayData)) \
    )
static const qt_meta_stringdata_ScintillaDocument_t qt_meta_stringdata_ScintillaDocument = {
    {
QT_MOC_LITERAL(0, 0, 17), // "ScintillaDocument"
QT_MOC_LITERAL(1, 18, 14), // "modify_attempt"
QT_MOC_LITERAL(2, 33, 0), // ""
QT_MOC_LITERAL(3, 34, 10), // "save_point"
QT_MOC_LITERAL(4, 45, 11), // "atSavePoint"
QT_MOC_LITERAL(5, 57, 8), // "modified"
QT_MOC_LITERAL(6, 66, 8), // "position"
QT_MOC_LITERAL(7, 75, 17), // "modification_type"
QT_MOC_LITERAL(8, 93, 4), // "text"
QT_MOC_LITERAL(9, 98, 6), // "length"
QT_MOC_LITERAL(10, 105, 10), // "linesAdded"
QT_MOC_LITERAL(11, 116, 4), // "line"
QT_MOC_LITERAL(12, 121, 12), // "foldLevelNow"
QT_MOC_LITERAL(13, 134, 13), // "foldLevelPrev"
QT_MOC_LITERAL(14, 148, 12), // "style_needed"
QT_MOC_LITERAL(15, 161, 3), // "pos"
QT_MOC_LITERAL(16, 165, 13), // "lexer_changed"
QT_MOC_LITERAL(17, 179, 14), // "error_occurred"
QT_MOC_LITERAL(18, 194, 6) // "status"

    },
    "ScintillaDocument\0modify_attempt\0\0"
    "save_point\0atSavePoint\0modified\0"
    "position\0modification_type\0text\0length\0"
    "linesAdded\0line\0foldLevelNow\0foldLevelPrev\0"
    "style_needed\0pos\0lexer_changed\0"
    "error_occurred\0status"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_ScintillaDocument[] = {

 // content:
       8,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       6,       // signalCount

 // signals: name, argc, parameters, tag, flags
       1,    0,   44,    2, 0x06 /* Public */,
       3,    1,   45,    2, 0x06 /* Public */,
       5,    8,   48,    2, 0x06 /* Public */,
      14,    1,   65,    2, 0x06 /* Public */,
      16,    0,   68,    2, 0x06 /* Public */,
      17,    1,   69,    2, 0x06 /* Public */,

 // signals: parameters
    QMetaType::Void,
    QMetaType::Void, QMetaType::Bool,    4,
    QMetaType::Void, QMetaType::Int, QMetaType::Int, QMetaType::QByteArray, QMetaType::Int, QMetaType::Int, QMetaType::Int, QMetaType::Int, QMetaType::Int,    6,    7,    8,    9,   10,   11,   12,   13,
    QMetaType::Void, QMetaType::Int,   15,
    QMetaType::Void,
    QMetaType::Void, QMetaType::Int,   18,

       0        // eod
};

void ScintillaDocument::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        auto *_t = static_cast<ScintillaDocument *>(_o);
        Q_UNUSED(_t)
        switch (_id) {
        case 0: _t->modify_attempt(); break;
        case 1: _t->save_point((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 2: _t->modified((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< const QByteArray(*)>(_a[3])),(*reinterpret_cast< int(*)>(_a[4])),(*reinterpret_cast< int(*)>(_a[5])),(*reinterpret_cast< int(*)>(_a[6])),(*reinterpret_cast< int(*)>(_a[7])),(*reinterpret_cast< int(*)>(_a[8]))); break;
        case 3: _t->style_needed((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 4: _t->lexer_changed(); break;
        case 5: _t->error_occurred((*reinterpret_cast< int(*)>(_a[1]))); break;
        default: ;
        }
    } else if (_c == QMetaObject::IndexOfMethod) {
        int *result = reinterpret_cast<int *>(_a[0]);
        {
            using _t = void (ScintillaDocument::*)();
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&ScintillaDocument::modify_attempt)) {
                *result = 0;
                return;
            }
        }
        {
            using _t = void (ScintillaDocument::*)(bool );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&ScintillaDocument::save_point)) {
                *result = 1;
                return;
            }
        }
        {
            using _t = void (ScintillaDocument::*)(int , int , const QByteArray & , int , int , int , int , int );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&ScintillaDocument::modified)) {
                *result = 2;
                return;
            }
        }
        {
            using _t = void (ScintillaDocument::*)(int );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&ScintillaDocument::style_needed)) {
                *result = 3;
                return;
            }
        }
        {
            using _t = void (ScintillaDocument::*)();
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&ScintillaDocument::lexer_changed)) {
                *result = 4;
                return;
            }
        }
        {
            using _t = void (ScintillaDocument::*)(int );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&ScintillaDocument::error_occurred)) {
                *result = 5;
                return;
            }
        }
    }
}

QT_INIT_METAOBJECT const QMetaObject ScintillaDocument::staticMetaObject = { {
    QMetaObject::SuperData::link<QObject::staticMetaObject>(),
    qt_meta_stringdata_ScintillaDocument.data,
    qt_meta_data_ScintillaDocument,
    qt_static_metacall,
    nullptr,
    nullptr
} };


const QMetaObject *ScintillaDocument::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *ScintillaDocument::qt_metacast(const char *_clname)
{
    if (!_clname) return nullptr;
    if (!strcmp(_clname, qt_meta_stringdata_ScintillaDocument.stringdata0))
        return static_cast<void*>(this);
    return QObject::qt_metacast(_clname);
}

int ScintillaDocument::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 6)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 6;
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        if (_id < 6)
            *reinterpret_cast<int*>(_a[0]) = -1;
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void ScintillaDocument::modify_attempt()
{
    QMetaObject::activate(this, &staticMetaObject, 0, nullptr);
}

// SIGNAL 1
void ScintillaDocument::save_point(bool _t1)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t1))) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void ScintillaDocument::modified(int _t1, int _t2, const QByteArray & _t3, int _t4, int _t5, int _t6, int _t7, int _t8)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t1))), const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t2))), const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t3))), const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t4))), const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t5))), const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t6))), const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t7))), const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t8))) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void ScintillaDocument::style_needed(int _t1)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t1))) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void ScintillaDocument::lexer_changed()
{
    QMetaObject::activate(this, &staticMetaObject, 4, nullptr);
}

// SIGNAL 5
void ScintillaDocument::error_occurred(int _t1)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t1))) };
    QMetaObject::activate(this, &staticMetaObject, 5, _a);
}
QT_WARNING_POP
QT_END_MOC_NAMESPACE
