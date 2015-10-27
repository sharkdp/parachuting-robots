// module Main

exports.onStep = function(f) {
    return function() {
        var handler = function() {
            var code = document.getElementById("code").value;
            f(code)();
        };

        document.getElementById("step").addEventListener("click", handler);
        document.addEventListener("keydown", function(e) {
            if (e.ctrlKey && e.keyCode == 13) {
                e.preventDefault();
                handler();
            }
        });

        return {};
    };
};

exports.printMessage = function(msg) {
    return function() {
        document.getElementById("output").innerHTML = msg;
        return {};
    };
};
