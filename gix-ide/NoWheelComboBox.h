#pragma once

#include <QComboBox>

class NoWheelComboBox : public QComboBox
{
public:
    using QComboBox::QComboBox;

    void wheelEvent(QWheelEvent *e)
    {
        if (hasFocus())
            QComboBox::wheelEvent(e);
    }

};

