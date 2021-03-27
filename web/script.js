function loadfile(editor, fn) {
    dir = "examples/";
    editor = editor[1]

    jQuery.ajax({
        type     : "GET",
        url      : dir + fn,
        dataType : 'text',
        success  : function (data) { editor.setValue(data); editor.clearSelection(); }
    });
}
