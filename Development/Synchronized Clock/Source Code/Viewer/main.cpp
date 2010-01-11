// Order is significant:
#include "resourceppc.h"
#include "stdafx.h"
#include <windows.h>
#include <commctrl.h>


#define MAX_LOADSTRING 100


HINSTANCE _currentApplicationInstance;
HWND _menuBarWindowHandle;


// Message handler for about box.
INT_PTR CALLBACK About(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam) {
    switch (message) {
    case WM_INITDIALOG: {
            // Create a Done button and size it.  
            SHINITDLGINFO shidi;
            shidi.dwMask = SHIDIM_FLAGS;
            shidi.dwFlags = SHIDIF_DONEBUTTON | SHIDIF_SIPDOWN | SHIDIF_SIZEDLGFULLSCREEN | SHIDIF_EMPTYMENU;
            shidi.hDlg = hDlg;
            SHInitDialog(&shidi);
        }
        return (INT_PTR) TRUE;
    case WM_COMMAND:
        if (LOWORD(wParam) == IDOK) {
            EndDialog(hDlg, LOWORD(wParam));
            return TRUE;
        }
        break;
    case WM_CLOSE:
        EndDialog(hDlg, message);
        return TRUE;
    }
    
    return (INT_PTR)FALSE;
}


//  PURPOSE:  Processes messages for the main window.
//
//  WM_COMMAND	- process the application menu
//  WM_PAINT	- Paint the main window
//  WM_DESTROY	- post a quit message and return
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) {
    int wmId, wmEvent;
    PAINTSTRUCT ps;
    HDC hdc;
    
    static SHACTIVATEINFO s_sai;
    
    switch (message) {
    case WM_COMMAND:
        wmId = LOWORD(wParam);
        wmEvent = HIWORD(wParam);
        // Parse the menu selections:
        switch (wmId) {
        case IDM_HELP_ABOUT:
            DialogBox(_currentApplicationInstance, (LPCTSTR) IDD_ABOUTBOX, hWnd, About);
            break;
        case IDM_OK:
            SendMessage(hWnd, WM_CLOSE, 0, 0);				
            break;
        default:
            return DefWindowProc(hWnd, message, wParam, lParam);
        }
        break;
    case WM_CREATE:
        SHMENUBARINFO mbi;
        
        memset(&mbi, 0, sizeof(SHMENUBARINFO));
        mbi.cbSize     = sizeof(SHMENUBARINFO);
        mbi.hwndParent = hWnd;
        mbi.nToolBarId = IDR_MENU;
        mbi.hInstRes   = _currentApplicationInstance;
        
        if (!SHCreateMenuBar(&mbi)) {
            _menuBarWindowHandle = NULL;
        }
        else {
            _menuBarWindowHandle = mbi.hwndMB;
        }
        
        // Initialize the shell activate info structure
        memset(&s_sai, 0, sizeof (s_sai));
        s_sai.cbSize = sizeof (s_sai);
        break;
    case WM_PAINT:
        hdc = BeginPaint(hWnd, &ps);
        
        // TODO: Add any drawing code here...
        
        EndPaint(hWnd, &ps);
        break;
    case WM_DESTROY:
        CommandBar_Destroy(_menuBarWindowHandle);
        PostQuitMessage(0);
        break;
    case WM_ACTIVATE:
        // Notify shell of our activate message
        SHHandleWMActivate(hWnd, wParam, lParam, &s_sai, FALSE);
        break;
    case WM_SETTINGCHANGE:
        SHHandleWMSettingChange(hWnd, wParam, lParam, &s_sai);
        break;
    default:
        return DefWindowProc(hWnd, message, wParam, lParam);
    }
    
    return 0;
}


//  PURPOSE: Registers the window class.
ATOM MyRegisterClass(HINSTANCE hInstance, LPTSTR szWindowClass) {
	WNDCLASS wc;
    
	wc.style = CS_HREDRAW | CS_VREDRAW;
	wc.lpfnWndProc = WndProc;
	wc.cbClsExtra = 0;
	wc.cbWndExtra = 0;
	wc.hInstance = hInstance;
	wc.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_VIEWER));
	wc.hCursor = 0;
	wc.hbrBackground = (HBRUSH) GetStockObject(WHITE_BRUSH);
	wc.lpszMenuName = 0;
	wc.lpszClassName = szWindowClass;
    
	return RegisterClass(&wc);
}


//   PURPOSE: Saves instance handle and creates main window
//        In this function, we save the instance handle in a global variable and
//        create and display the main program window.
BOOL InitInstance(HINSTANCE hInstance, int nCmdShow) {
    HWND hWnd;
    TCHAR szTitle[MAX_LOADSTRING];		// title bar text
    TCHAR szWindowClass[MAX_LOADSTRING];	// main window class name
    
    _currentApplicationInstance = hInstance; // Store instance handle in our global variable
    
    // SHInitExtraControls should be called once during your application's initialization to initialize any
    // of the device specific controls such as CAPEDIT and SIPPREF.
    SHInitExtraControls();
    
    LoadString(hInstance, IDS_APP_TITLE, szTitle, MAX_LOADSTRING); 
    LoadString(hInstance, IDC_VIEWER, szWindowClass, MAX_LOADSTRING);
    
    //If it is already running, then focus on the window, and exit
    hWnd = FindWindow(szWindowClass, szTitle);	
    
    if (hWnd) {
        // set focus to foremost child window
        // The "| 0x00000001" is used to bring any owned windows to the foreground and
        // activate them.
        SetForegroundWindow((HWND)((ULONG) hWnd | 0x00000001));
        return 0;
    } 
    
    if (!MyRegisterClass(hInstance, szWindowClass)) {
    	return FALSE;
    }
    
    hWnd = CreateWindow(szWindowClass, szTitle, WS_VISIBLE,
        CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, NULL, NULL, hInstance, NULL);
    
    if (!hWnd) {
        return FALSE;
    }
    
    // When the main window is created using CW_USEDEFAULT the height of the menubar (if one
    // is created is not taken into account). So we resize the window after creating it
    // if a menubar is present
    if (_menuBarWindowHandle) {
        RECT rc;
        RECT rcMenuBar;
        
        GetWindowRect(hWnd, &rc);
        GetWindowRect(_menuBarWindowHandle, &rcMenuBar);
        rc.bottom -= (rcMenuBar.bottom - rcMenuBar.top);
	    
        MoveWindow(hWnd, rc.left, rc.top, rc.right-rc.left, rc.bottom-rc.top, FALSE);
    }
    
    ShowWindow(hWnd, nCmdShow);
    UpdateWindow(hWnd);

    return TRUE;
}


int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow) {
	MSG msg;
    
	// Perform application initialization:
	if (!InitInstance(hInstance, nCmdShow)) {
		return FALSE;
	}
    
	HACCEL hAccelTable;
	hAccelTable = LoadAccelerators(hInstance, MAKEINTRESOURCE(IDC_VIEWER));
    
	// Main message loop:
	while (GetMessage(&msg, NULL, 0, 0)) {
		if (!TranslateAccelerator(msg.hwnd, hAccelTable, &msg)) {
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}
    
	return (int) msg.wParam;
}
