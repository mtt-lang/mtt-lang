ace.define("ace/mode/mtt_highlighting_rules", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text_highlight_rules"], function (require, exports, module) {
    "use strict";
    var oop = require("../lib/oop");
    var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

    var MttHighlightRules = function () {
        
        this.$rules = {
            "start": [
                {
                    token: "keyword",
                    regex: "let|letbox|in|fst|snd|box|fun"
                },
                {
                    token: "keyword.operator",
                    regex: "\\[\\]|=>|->|\\*|<|>|λ"
                }
            ]
        
        };

        this.normalizeRules();
    };

    oop.inherits(MttHighlightRules, TextHighlightRules);
    exports.MttHighlightRules = MttHighlightRules;
});

ace.define("ace/mode/mtt", ["require", "exports", "module", "ace/lib/oop", "ace/mode/text", "ace/mode/mtt_highlighting_rules"], function (require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextMode = require("./text").Mode;
    var MttHighlightRules = require("./mtt_highlighting_rules").MttHighlightRules;

    var Mode = function () {
        this.HighlightRules = MttHighlightRules;
        this.$behaviour = this.$defaultBehaviour;
    };
    oop.inherits(Mode, TextMode);

    (function () {

        this.lineCommentStart = "#";

        this.$id = "ace/mode/mtt";
        this.snippetFileId = "ace/snippets/mtt";
    }).call(Mode.prototype);

    exports.Mode = Mode;

}); (function () {
    ace.require(["ace/mode/mtt"], function (m) {
        if (typeof module == "object" && typeof exports == "object" && module) {
            module.exports = m;
        }
    });
})();