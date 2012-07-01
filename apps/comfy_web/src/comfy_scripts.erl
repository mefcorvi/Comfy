-module(comfy_scripts).
-export([get_scripts_list/0, combine_scripts/0, combine_styles/0]).
-include("comfy_web_log.hrl").

get_scripts_list() ->
    [
     "external/jquery.js",
     "external/jquery.cookie.js",
     "external/jquery.bgiframe.js",
     "external/jquery.browser.js",
     "external/jquery.history.js",
     "external/jquery.mousewheel.js",
     "external/jquery.dynDateTime.js",
     "external/jquery-ui-datePicker.min.js",
     "external/jscolor.js",
     "external/hashtable.js",
     "services/json.js",
     "common/base64.js",
     "common/debug.js",
     "common/function.js",
     "common/object.js",
     "common/guid.js",
     "common/method-interceptor.js",
     "common/proxy-interceptor.js",

     "common/data/array.js",
     "common/data/array.linq.js",

     "common/auto.event.js",
     "common/auto.property.js",

     "common/trait.js",
     "common/data/array.observable.js",
     "common/data/array.sorted.js",
     "common/data/array.tracking.js",

     "common/inheritance.js",
     "common/string-builder.js",
     "common/regexp.js",
     "common/string.js",
     "common/boolean.js",
     "common/date.js",
     "common/culture.js",
     "common/dom.js",

     "core/queue-processor.js",
     "data/socket.js",
     "data/dto-processor.js",
     "data/entity.js",
     "data/entity-set.js",
     "data/data-filter.js",
     "data/data-source.js",
     "data/data-source.filtered.js",
     "data/data-source.paginated.js",
     "data/data-source.ordered.js",

     "history/history.observer.js",
     "history/history.base-observer.js",
     "history/history.default-observer.js",
     "history/history.data-source.js",
     "history/history.observable-list.js",
     "history/history.boolean.js",
     "history/history.date.js",

     "services/pages-service.js",
     "services/session.js",

     "core/keys.js",
     "core/ui/text-measurer.js",
     "core/ui/css-rules.js",
     "core/ui/dimension-unit.js",
     "core/ui/rect.js",
     "core/page-uri.js",
     "core/application-context.js",
     "core/application.js",
     "core/configuration.js",
     "core/ui/layout-engines/layout-factory.js",
     "core/ui/depth-manager.js",
     "core/ui/color.js",
     "core/ui/layout-engines/base-layout.js",
     "core/ui/layout-engines/stack-layout.js",
     "core/ui/layout-engines/*.js",
     "core/ui/control.js",
     "core/ui/controls-collection.js",
     "core/ui/controls-factory.js",
     "core/ui/template.js",
     "core/ui/base-list-data-control.js",
     "commands/*.js"
    ].

get_controls_list() ->
    [
     {control, "Panel"},
     {control, "Repeater"},
     {control, "Button"},
     {control, "Checkbox"},
     {control, "CheckList"},
     {control, "CollapsablePanel"},
     {control, "Container"},
     {control, "DataGrid"},
     {control, "DatePicker"},
     {control, "DropDownList"},
     {control, "FileUpload"},
     {control, "PhotoUpload"},
     {control, "Image"},
     {control, "Label"},
     {control, "Link"},
     {control, "ListView"},
     {control, "Literal"},
     {control, "Page"},
     {control, "Frame"},
     {control, "Form"},
     {control, "Pager"},
     {control, "FancyBox"},
     {control, "Popup"},
     {control, "ScrollablePanel"},
     {control, "Silverlight"},
     {control, "Slider"},
     {control, "Table"},
     {control, "TabPanel"},
     {control, "TextBox"},
     {control, "TextEditor"},
     {control, "Tree"},
     {control, "Tooltip"},
     {control, "Wizard"},
     {control, "ColorPicker"},
     {control, "MultiView"},
     {control, "HistoryTextBox"}
    ].

combine_scripts() ->
    FileName = comfy_web_config:get(docroot) ++ "scripts.js",
    ?Log("Merging scripts into ~p...", [FileName]),
    {ok, Dest} = file:open(FileName, [write]),
    append_scripts(Dest, get_scripts_list()),
    append_controls(Dest, get_controls_list()),
    ok = file:close(Dest).

append_scripts(Dest, Scripts) ->
    %% makes all paths are absolute
    Files = lists:map(fun get_full_path/1, Scripts),   
    %% append files
    lists:foldl(fun(Item, AccIn) -> append_file(Dest, Item, AccIn) end, sets:new(), Files).

append_controls(Dest, Controls) ->
    %% get scripts names
    Scripts = lists:foldl(fun({control, ControlName}, AccIn) ->
				  AccIn ++
				      [lists:concat(["../controls/", ControlName, "/", ControlName, ".js"])] ++
				      [lists:concat(["../controls/", ControlName, "/*.js"])]
			  end, [], Controls),
    append_scripts(Dest, Scripts).

append_file(Dest, FileName, DestSet) ->
    case filelib:is_regular(FileName) of
	true ->
	    case sets:is_element(FileName, DestSet) of
		true ->
		    DestSet;
		false ->
		    append_file(Dest, FileName),
		    sets:add_element(FileName, DestSet)
	    end;
	false ->
	    Files = filelib:wildcard(FileName),
	    lists:foldl(fun(Item, AccIn) -> append_file(Dest, Item, AccIn) end, DestSet, Files)
    end.

append_file(Dest, FileName) when is_list(FileName) ->
    {ok, Result} = file:read_file(FileName),
    case unicode:bom_to_encoding(Result) of
	{_, 0} -> ok;
	_ -> io:format("Warning! File ~p contains BOM.~n", [FileName])
    end,
    file:write(Dest, <<10,13>>),
    ok = file:write(Dest, Result).

get_full_path(FileName) ->
    comfy_web_config:get(scripts_root) ++ FileName.

combine_styles() ->
    FileName = comfy_web_config:get(docroot) ++ "styles.css",
    ?Log("Merging styles into ~p...", [FileName]),
    {ok, Dest} = file:open(FileName, [write]),
    append_styles(Dest, comfy_web_config:get(themes_root)),
    append_styles(Dest, comfy_web_config:get(controls_root)),
    ok = file:close(Dest).

append_styles(Dest, Path) ->
    ListOfStyles = filelib:fold_files(Path, ".*?\.css", true, fun(FileName, AccIn) -> AccIn ++ [FileName] end, []),
    lists:foldl(fun(Item, AccIn) -> append_style(Dest, Item, AccIn) end, sets:new(), ListOfStyles).

append_style(Dest, FileName, FilesSet) ->
    case sets:is_element(FileName, FilesSet) of
	true ->
	    FilesSet;
	false ->
	    {ok, Result} = file:read_file(FileName),
	    case unicode:bom_to_encoding(Result) of
		{_, 0} -> ok;
		_ -> io:format("Warning! File ~p contains BOM.~n", [FileName])
	    end,
	    Replacement = lists:concat(["url(\"", get_url_to_directory(FileName), "/\\2\")"]),
	    NewResult = re:replace(Result, "url\\((\"|')?~/?(.+?)(\"|')?\\)", Replacement, [global]),
	    file:write(Dest, <<10,13>>),
	    ok = file:write(Dest, NewResult),
	    sets:add_element(FileName, FilesSet)
    end.

get_url_to_directory(FileName) ->
    "/" ++ filename:dirname(re:replace(FileName, comfy_web_config:get(docroot), "", [{return, list}])).
