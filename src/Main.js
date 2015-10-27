// module Main

exports.toggleInterval = (function() {
    var id;
    return function() {
        if (id === undefined) {
            id = setInterval(function() {
                document.getElementById("step").click();
            }, 100);
        } else {
            clearInterval(id);
            id = undefined;
        }

        return {};
    };
})();
