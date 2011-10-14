var Color = {
    GetForeground: function (backgroundColor) {
        var 
            re=/^\#(\w{2})(\w{2})(\w{2})$/,
            rgb=re.exec(backgroundColor);

        var 
            r=parseInt(rgb[1],16),
            g=parseInt(rgb[2],16),
            b=parseInt(rgb[3],16);

        var brightness=(r*299+g*587+b*114)/1000;

        if(brightness>125) {
            return "#000000";
        } else {
            return "#FFFFFF";
        }
    },

    GetRandom: function () {
        var rint = Math.round(0xffffff * (Math.random() * 0.7 + 0.3));
        return ('#0'+rint.toString(16)).replace(/^#0([0-9a-f]{6})$/i,'#$1');
    }
};