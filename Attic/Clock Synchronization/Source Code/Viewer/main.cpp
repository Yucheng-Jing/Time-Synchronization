#include "resources.h"
#include "Win32.h"


#define SK_EXIT Win32::LoadStringT(IDS_SK_EXIT)
#define SK_UPDATE Win32::LoadStringT(IDS_SK_UPDATE)
#define TITLE Win32::LoadStringT(IDS_TITLE)
#define WINDOW_CLASS Win32::LoadStringT(IDS_WINDOW_CLASS)


class Viewer : public Win32::Application, Win32::Window {
public:
    Viewer(HINSTANCE instance)
        : Win32::Application(instance), Win32::Window(TITLE, WINDOW_CLASS) {
    }


protected:
    virtual void onStart(int windowShowMode) {
        ref<Win32::Menu> menuBar = new Win32::Menu();

        menuBar->addItem(new Win32::MenuItem(SK_UPDATE));
        menuBar->addItem(new Win32::MenuItem(SK_EXIT));
        
        addMenuBar(menuBar);
        show(windowShowMode);
    }
};


int WINAPI WinMain(
    HINSTANCE instance,
    HINSTANCE previousInstance,
    LPTSTR commandLine,
    int windowShowMode)
{
    try {
        return Viewer(instance).start(windowShowMode);
    }
    catch (Win32::Exception exception) {
        Win32::ErrorMessageBox(exception.getMessage());
        return EXIT_FAILURE;
    }
}


/*
        CreateWindow(
            L"BUTTON",   // Predefined class; Unicode assumed.
            L"OK",       // Button text. 
            WS_TABSTOP | WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON,  // Styles.
            10,         // x position. 
            10,         // y position. 
            100,        // Button width.
            100,        // Button height.
            getHandle(),       // Parent window.
            NULL,       // No menu.
            application, 
            NULL);      // Pointer not needed.
*/

/*
        HMENU menuBar = CreateMenu();
        
        ref<Win32::String> update = Win32::LoadStringT(IDS_SK_UPDATE);
        ref<Win32::String> exit = Win32::LoadStringT(IDS_SK_EXIT);
        
        InsertMenu(menuBar, -1, MF_BYPOSITION + MF_STRING, IDS_SK_UPDATE, update->c_str());
        InsertMenu(menuBar, -1, MF_BYPOSITION + MF_STRING, IDS_SK_EXIT, exit->c_str());

        SHMENUBARINFO menuBarInfo;

        ZeroMemory(&menuBarInfo, sizeof(SHMENUBARINFO));
        menuBarInfo.cbSize = sizeof(SHMENUBARINFO);
        menuBarInfo.hwndParent = Win32::Window::getHandle();
        menuBarInfo.dwFlags = SHCMBF_HMENU | SHCMBF_HIDESIPBUTTON;
        menuBarInfo.nToolBarId = (UINT) menuBar;
        menuBarInfo.hInstRes = GetModuleHandle(NULL);
        
        if (!SHCreateMenuBar(&menuBarInfo)) {
            throw Win32::Exception(Win32::GetLastErrorMessage());
        }
*/

/*
        switch (LOWORD(wParam)) {
        case IDS_SK_EXIT:
            SendMessage(handle, WM_CLOSE, 0, 0);
            break;
        case IDS_SK_UPDATE:
            break;
        default:
            return DefWindowProc(handle, message, wParam, lParam);
        }
*/
