var config = exports;

config['Browser Tests'] = {
    environment: 'browser',
    sources: [],
    tests: [ "resources/js/zetta-parser-browser-test.js" ]
};

config['Node Tests'] = {
    environment: 'node',
    sources: [],
    tests: [ "resources/js/zetta-parser-node-test.js" ]
};
