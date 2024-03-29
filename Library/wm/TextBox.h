#pragma once


#include "Exception.h"
#include "Control.h"


namespace wm {
    class TextBox: public Control {
    public:
        TextBox(String text = S("")):
            Control(S("EDIT"), text, WS_BORDER + ES_AUTOHSCROLL)
        {
        }


        virtual bool isReadOnly() {
            LONG style = GetWindowLong(getWidgetHandle(), GWL_STYLE);

            if (style == 0) {
                Exception::throwLastError();
            }

            return (style & ES_READONLY) != 0;
        }


        virtual void setReadOnly(bool readOnly) {
            if (!SendMessage(getWidgetHandle(), EM_SETREADONLY, readOnly, 0)) {
                Exception::throwLastError();
            }
        }
    };
}
