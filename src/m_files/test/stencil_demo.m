function varargout = stencil_demo(varargin)
% STENCIL_DEMO MATLAB code for stencil_demo.fig
%      STENCIL_DEMO, by itself, creates a new STENCIL_DEMO or raises the existing
%      singleton*.
%
%      H = STENCIL_DEMO returns the handle to a new STENCIL_DEMO or the handle to
%      the existing singleton*.
%
%      STENCIL_DEMO('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in STENCIL_DEMO.M with the given input arguments.
%
%      STENCIL_DEMO('Property','Value',...) creates a new STENCIL_DEMO or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before stencil_demo_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to stencil_demo_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help stencil_demo

% Last Modified by GUIDE v2.5 01-Aug-2012 10:37:45

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @stencil_demo_OpeningFcn, ...
                   'gui_OutputFcn',  @stencil_demo_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before stencil_demo is made visible.
function stencil_demo_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to stencil_demo (see VARARGIN)

% Choose default command line output for stencil_demo
handles.output = hObject;

% Read in the image
handles.pic0 = imread('photo1.jpg'); % original image
handles.pic1 = imread('photo1.jpg'); % current image

% Display the image in axes
updateDisplay(handles);

% Set the FontSize, Callbacks, and contained text
children = get(handles.stencil_panel,'Children');
children = flipud(children); 

n = floor(sqrt(length(children)));

for i = 1:length(children)
    set(children(i),'FontSize',6.0);
    set(children(i),'FontName','tahoma');
    set(children(i),'Callback',{@userModifyHandler,handles});
end

% Set the initial stencil
stencil = zeros(n,n);
stencil(ceil(n/2),ceil(n/2)) = 1;
handles.stencil=stencil;
setStencilText(handles);

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes stencil_demo wait for user response (see UIRESUME)
% uiwait(handles.stencil_demo);


% --- Outputs from this function are returned to the command line.
function varargout = stencil_demo_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;

% --- Executes on button press in apply.
function apply_Callback(hObject, eventdata, handles)
% hObject    handle to apply (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

try
    if( get(handles.stencil_panel,'UserData') ) % check the flag to see if the user has entered their own stencil
        handles.stencil=getUserStencil(handles);
        set(handles.stencil_panel,'UserData',false); % clear the flag
    end

    stencil=handles.stencil;

    stencil=rot90(stencil,2); % convolution vs straightforward stencil

    pic=handles.pic1;

    % TODO: could probably use imfilter instead of conv2 (with appropriate
    % un-flipping of stencil.
    pic2(:,:,1)=conv2(double(pic(:,:,1)),stencil,'same');
    pic2(:,:,2)=conv2(double(pic(:,:,2)),stencil,'same');
    pic2(:,:,3)=conv2(double(pic(:,:,3)),stencil,'same');
    pic2=max(pic2,zeros(size(pic2)));
    pic2=min(pic2,255*ones(size(pic2)));
    pic2=uint8(pic2);

    handles.pic1=pic2;
    updateDisplay(handles);
    guidata(hObject,handles);
catch err
   waitfor(msgbox(err.message,'Error','error')); 
end





function stencil_11_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_11 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_11 as text
%        str2double(get(hObject,'String')) returns contents of stencil_11 as a double


% --- Executes during object creation, after setting all properties.
function stencil_11_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_11 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_12_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_12 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_12 as text
%        str2double(get(hObject,'String')) returns contents of stencil_12 as a double


% --- Executes during object creation, after setting all properties.
function stencil_12_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_12 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_13_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_13 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_13 as text
%        str2double(get(hObject,'String')) returns contents of stencil_13 as a double


% --- Executes during object creation, after setting all properties.
function stencil_13_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_13 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_14_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_14 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_14 as text
%        str2double(get(hObject,'String')) returns contents of stencil_14 as a double


% --- Executes during object creation, after setting all properties.
function stencil_14_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_14 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_15_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_15 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_15 as text
%        str2double(get(hObject,'String')) returns contents of stencil_15 as a double


% --- Executes during object creation, after setting all properties.
function stencil_15_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_15 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_21_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_21 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_21 as text
%        str2double(get(hObject,'String')) returns contents of stencil_21 as a double


% --- Executes during object creation, after setting all properties.
function stencil_21_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_21 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_22_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_22 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_22 as text
%        str2double(get(hObject,'String')) returns contents of stencil_22 as a double


% --- Executes during object creation, after setting all properties.
function stencil_22_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_22 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_23_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_23 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_23 as text
%        str2double(get(hObject,'String')) returns contents of stencil_23 as a double


% --- Executes during object creation, after setting all properties.
function stencil_23_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_23 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_24_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_24 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_24 as text
%        str2double(get(hObject,'String')) returns contents of stencil_24 as a double


% --- Executes during object creation, after setting all properties.
function stencil_24_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_24 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_25_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_25 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_25 as text
%        str2double(get(hObject,'String')) returns contents of stencil_25 as a double


% --- Executes during object creation, after setting all properties.
function stencil_25_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_25 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_31_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_31 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_31 as text
%        str2double(get(hObject,'String')) returns contents of stencil_31 as a double


% --- Executes during object creation, after setting all properties.
function stencil_31_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_31 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_32_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_32 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_32 as text
%        str2double(get(hObject,'String')) returns contents of stencil_32 as a double


% --- Executes during object creation, after setting all properties.
function stencil_32_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_32 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_33_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_33 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_33 as text
%        str2double(get(hObject,'String')) returns contents of stencil_33 as a double


% --- Executes during object creation, after setting all properties.
function stencil_33_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_33 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_34_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_34 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_34 as text
%        str2double(get(hObject,'String')) returns contents of stencil_34 as a double


% --- Executes during object creation, after setting all properties.
function stencil_34_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_34 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_35_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_35 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_35 as text
%        str2double(get(hObject,'String')) returns contents of stencil_35 as a double


% --- Executes during object creation, after setting all properties.
function stencil_35_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_35 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_41_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_41 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_41 as text
%        str2double(get(hObject,'String')) returns contents of stencil_41 as a double


% --- Executes during object creation, after setting all properties.
function stencil_41_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_41 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_42_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_42 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_42 as text
%        str2double(get(hObject,'String')) returns contents of stencil_42 as a double


% --- Executes during object creation, after setting all properties.
function stencil_42_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_42 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_43_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_43 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_43 as text
%        str2double(get(hObject,'String')) returns contents of stencil_43 as a double


% --- Executes during object creation, after setting all properties.
function stencil_43_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_43 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_44_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_44 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_44 as text
%        str2double(get(hObject,'String')) returns contents of stencil_44 as a double


% --- Executes during object creation, after setting all properties.
function stencil_44_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_44 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_45_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_45 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_45 as text
%        str2double(get(hObject,'String')) returns contents of stencil_45 as a double


% --- Executes during object creation, after setting all properties.
function stencil_45_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_45 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_51_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_51 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_51 as text
%        str2double(get(hObject,'String')) returns contents of stencil_51 as a double


% --- Executes during object creation, after setting all properties.
function stencil_51_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_51 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_52_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_52 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_52 as text
%        str2double(get(hObject,'String')) returns contents of stencil_52 as a double


% --- Executes during object creation, after setting all properties.
function stencil_52_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_52 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_53_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_53 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_53 as text
%        str2double(get(hObject,'String')) returns contents of stencil_53 as a double


% --- Executes during object creation, after setting all properties.
function stencil_53_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_53 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_54_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_54 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_54 as text
%        str2double(get(hObject,'String')) returns contents of stencil_54 as a double


% --- Executes during object creation, after setting all properties.
function stencil_54_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_54 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function stencil_55_Callback(hObject, eventdata, handles)
% hObject    handle to stencil_55 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of stencil_55 as text
%        str2double(get(hObject,'String')) returns contents of stencil_55 as a double


% --- Executes during object creation, after setting all properties.
function stencil_55_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_55 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes during object creation, after setting all properties.
function display1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to display1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: place code in OpeningFcn to populate display1


% --- Executes during object creation, after setting all properties.
function stencil_panel_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_panel (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called


% --- If Enable == 'on', executes on mouse press in 5 pixel border.
% --- Otherwise, executes on mouse press in 5 pixel border or over apply.
function apply_ButtonDownFcn(hObject, eventdata, handles)
% hObject    handle to apply (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in reset_button.
function reset_button_Callback(hObject, eventdata, handles)
% hObject    handle to reset_button (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
pic1=handles.pic0;
handles.pic1=pic1;

axes(handles.display1);
pic1 = flipdim(pic1,1); % this is needed because image reverses the row order
image('CData',pic1);
axis image;
guidata(hObject,handles);


% --- Executes on selection change in popupmenu1.
function popupmenu1_Callback(hObject, eventdata, handles)
% hObject    handle to popupmenu1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns popupmenu1 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from popupmenu1

contents = cellstr(get(hObject,'String'));
index = get(hObject,'Value');

str = contents{index};
switch(str);
    case 'Preset Stencils...'
        % do nothing
    case 'Identity (1pt)'
        stencil=[ 0 0 0 0 0;
                  0 0 0 0 0;
                  0 0 1 0 0;
                  0 0 0 0 0;
                  0 0 0 0 0;];
    case 'Laplacian (5pt)'
        stencil = ...
            [ 0 0  0 0 0;
              0 0  1 0 0;
              0 1 -4 1 0;
              0 0  1 0 0;
              0 0  0 0 0;];
    case 'Laplacian (9pt)'
        stencil = ...
            [ 0 0  0  0 0;
              0 1  4  1 0;
              0 4 -20 4 0;
              0 1  4  1 0;
              0 0  0  0 0;]/6;
    case 'Biharmonic (13pt)'
        stencil = ...
            [ 0  0  1  0 0;
              0  2 -8  2 0;
              1 -8 20 -8 1;
              0  2 -8  2 0;
              0  0  1  0 0;];
    case 'Simpson''s Rule (9pt)'
        stencil = ...
                [ 0 0  0 0 0;
                  0 1  4 1 0;
                  0 4 16 4 0;
                  0 1  4 1 0;
                  0 0  0 0 0;]/9;
    case 'Gaussian Blur (9pt)'
        [x,y]=meshgrid(-2:1:2,-2:1:2);
        rsq=x.^2+y.^2;
        sigsq=0.25;
        gaussian=exp(-0.5.*rsq./sigsq)./(2.*pi.*sigsq);
        stencil=gaussian;children = get(handles.stencil_panel,'Children');
children = flipud(children);
n = floor(sqrt(length(children)));

for i = 1:length(children)
    q = floor((i-1)./n); % i-1 for 0-based indexing
    row_index = q+1; % quotient+1 gives row index
    col_index = mod(i-1,n)+1; % i-1 mod n gives column index
    value = stencil(row_index,col_index);
    set(children(i),'String',sprintf('%.3f',value));
end

end

if( index ~= 1)
    handles.stencil=stencil;
    setStencilText(handles);
end



guidata(hObject,handles);


% --- Executes during object creation, after setting all properties.
function popupmenu1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to popupmenu1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLABPush Button
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes during object creation, after setting all properties.
function stencil_demo_CreateFcn(hObject, eventdata, handles)
% hObject    handle to stencil_demo (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called


% --- Executes on button press in file_dialog.
function file_dialog_Callback(hObject, eventdata, handles)
% hObject    handle to file_dialog (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

[fname,cancelled]=imgetfile;
if(~cancelled)
    rgb=imread(fname);
    handles.pic0=rgb;
    handles.pic1=rgb;
    cla(handles.display1,'reset');
    updateDisplay(handles);
end

guidata(hObject,handles);

function updateDisplay( handles )
pic1=handles.pic1;
pic1 = flipdim(pic1,1); % this is needed because image reverses the row order
image('CData',pic1,'Parent',handles.display1);
axis image;


function stencil = getUserStencil(handles)
children = get(handles.stencil_panel,'Children');
children = flipud(children); 
% the above line is needed because handles are stored as a stack, i.e. in
% the reverse order of the creation/naming order, i.e. stencil_55 is 1st
n = round(sqrt(length(children)));
stencil = zeros(n,n);
for i = 1:length(children)
    q = floor((i-1)./n); % i-1 for 0-based indexing
    row_index = q+1; % quotient+1 gives row index
    col_index = mod(i-1,n)+1; % i-1 mod n gives column index
    val=str2double(get(children(i),'String'));
    assert(~isnan(val) && isreal(val) && ~isinf(val),...
        'Incorrect Stencil Value!');
    stencil(row_index,col_index)=val;
end

function setStencilText( handles )
stencil = handles.stencil;
children = get(handles.stencil_panel,'Children');
children = flipud(children);
% the above line is needed because handles are stored as a stack, i.e. in
% the reverse order of the creation/naming order, i.e. stencil_55 is 1st
n = floor(sqrt(length(children)));

for i = 1:length(children)
    q = floor((i-1)./n); % i-1 for 0-based indexing
    row_index = q+1; % quotient+1 gives row index
    col_index = mod(i-1,n)+1; % i-1 mod n gives column index
    value = stencil(row_index,col_index);
    set(children(i),'String',sprintf('%.3f',value));
end

function userModifyHandler(hObject, eventdata, handles)
set(handles.stencil_panel,'UserData',true); % set the modification flag
guidata(hObject,handles);
