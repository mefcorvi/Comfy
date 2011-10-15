///////////////////////////////////////////////////////////////////////////////
// Boolean Extensions

Boolean.parse = function(str) {
    if (isBoolean(str))
        return str;

    if (str === 'true') 
        return true;

    if (str === 'false')
        return false;
    
    throw new Error('Cannot parse bool. Valid values are "true", "false"');
};