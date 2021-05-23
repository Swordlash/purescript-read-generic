exports._readDateTime = function(str) {
    return function (ctor) {
        return function(just) {
            return function(nothing) {
                epoch = Date.parse(str);
                if (isNaN(epoch)) return nothing;
                else {
                    dt = new Date(epoch);
                    return just(ctor (dt.getUTCFullYear())(dt.getUTCMonth() + 1)(dt.getUTCDate())(dt.getUTCHours())(dt.getUTCMinutes())(dt.getUTCSeconds())(dt.getUTCMilliseconds()));
                }
            };
        };
    };
};
