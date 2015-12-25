%%
%%  wings_vbo.erl --
%%
%%     Utilities for handling Vertex Buffer Objects.
%%
%%  Copyright (c) 2015 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_vbo).
-export([draw/2,draw/3,new/2,new/3]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foldl/3]).

new(Draw, Data) ->
    new(Draw, Data, [vertex]).

new(Draw, Data0, Layout) when is_list(Data0) ->
    Data = << <<A:?F32,B:?F32,C:?F32>> || {A,B,C} <- Data0 >>,
    new(Draw, Data, Layout);
new(Draw, Data, Layout) when is_binary(Data) ->
    [Vbo] = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
    gl:bufferData(?GL_ARRAY_BUFFER, byte_size(Data), Data, ?GL_STATIC_DRAW),
    gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
    D = draw_fun(Layout, Vbo, Draw),
    {call,D,{vbo,Vbo}}.

draw(Draw, Data) ->
    draw(Draw, Data, [vertex]).

draw(Draw0, Data0, Layout) ->
    Data = << <<A:?F32,B:?F32,C:?F32>> || {A,B,C} <- Data0 >>,
    [Vbo] = Buffers = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
    gl:bufferData(?GL_ARRAY_BUFFER, byte_size(Data), Data, ?GL_STATIC_DRAW),
    Draw = draw_fun(Layout, Vbo, Draw0),
    Draw(),
    gl:deleteBuffers(Buffers),
    ok.

draw_fun(Layout, Vbo, Draw0) ->
    Stride = stride(Layout),
    Draw = draw_fun_1(Layout, Stride, Draw0, 0),
    fun() ->
	    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
	    Draw(),
	    gl:bindBuffer(?GL_ARRAY_BUFFER, 0)
    end.

draw_fun_1([Type|T], Stride, Fun0, Addr0) ->
    Fun = draw_fun_2(Type, Stride, Fun0, Addr0),
    Addr = Addr0 + width(Type),
    draw_fun_1(T, Stride, Fun, Addr);
draw_fun_1([], _, Fun, _) -> Fun.

draw_fun_2(vertex, Stride, Fun, Addr) ->
    fun() ->
	    gl:vertexPointer(3, ?GL_FLOAT, Stride, Addr),
	    gl:enableClientState(?GL_VERTEX_ARRAY),
	    Fun(),
	    gl:disableClientState(?GL_VERTEX_ARRAY)
    end;
draw_fun_2(color, Stride, Fun, Addr) ->
    fun() ->
	    gl:colorPointer(3, ?GL_FLOAT, Stride, Addr),
	    gl:enableClientState(?GL_COLOR_ARRAY),
	    Fun(),
	    gl:disableClientState(?GL_COLOR_ARRAY)
    end;
draw_fun_2(uv, Stride, Fun, Addr) ->
    fun() ->
	    gl:texCoordPointer(2, ?GL_FLOAT, Stride, Addr),
	    gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),
	    Fun(),
	    gl:disableClientState(?GL_TEXTURE_COORD_ARRAY)
    end.

stride([_]) ->
    0;
stride(L) ->
    foldl(fun(Item, Sum) ->
		  Sum + width(Item)
	  end, 0, L).

width(vertex) -> 3*4;
width(color) -> 3*4;
width(uv) -> 2*4.
