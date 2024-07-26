/******* GLOBAL VARIABLES ****************************************************************/
var fs = require('fs');
var express = require('express');
const router = express.Router();
const path = require('path');
var R = require("r-script");
var bodyParser = require('body-parser');
const MongoClient = require('mongodb').MongoClient;
var session = require('express-session');
var MongoDBStore = require('connect-mongodb-session')(session);
/******************************* APP ****************************************/

var app = express();
module.exports = app;

app.listen(8080, function () {
});

app.use(bodyParser.urlencoded({extended: false}));
app.use(bodyParser.json());
app.set("view engine", "pug");
app.set("views", path.join(__dirname, "views"));
app.use("/", router);
app.use(express.static(__dirname + '/public'));

var mongoClient;
// TODO add MongoClient
MongoClient.connect('link_to_client', (err, client) => {
    if (err) return console.error(err)
    mongoClient = client;
});

var store = new MongoDBStore({
    uri: 'your_mongo_srv', //TODO add server info
    collection: 'mySessions',
    ttl: 15 * 60,
    autoRemove: 'native'
});

app.get('/', function (req, res) {
    res.sendFile(__dirname + "/index.html");
});

// Catch errors
store.on('error', function (error) {
    console.log(error);
});

/** feature names*/
var featureNames = [];

/** the data extracted from the text file */
var dataInstances = [];

var columnNames = ["no", "q_type", "vq", "dur", "prenuc", "nuc", "edge_tone", "item", "pre", "precont", "itemname", "filename", "filename_with_extension"];

/** read the data file, get all wav file titles */
var readCount = 0;
readData = function (data) {
    var instanceArray = data.split("\n");
    featureNames = [];
    dataInstances = [];

    // first instance contain the feature names
    featureNames = instanceArray[0].split("\t");

    // remove the feature names
    instanceArray.splice(0, 1);

    // the remaining instances contain actual values
    instanceArray.forEach(function (stringInstance) {
        var attributes = stringInstance.split("\t");
        var instance = {};
        instance.id = parseInt(attributes[columnNames.indexOf("no")]);
        instance.q_type = attributes[columnNames.indexOf("q_type")];
        instance.item = attributes[columnNames.indexOf("item")];
        instance.itemname = attributes[columnNames.indexOf("itemname")].trim();
        instance.filename = attributes[columnNames.indexOf("filename")].trim();
        instance.filename_with_extension = attributes[columnNames.indexOf("filename_with_extension")].trim();
        instance.label = "NOT_SPECIFIED_LABEL";
        instance.predicted_label = "NOT_SPECIFIED_PREDICTED_LABEL";
        instance.prediction_probability = 0.5;
        instance.labeled = false;
        instance.cue_vector = [];
        attributes.forEach(function (attribute, i) {
            if (i > 0 && i < 7) {
                instance.cue_vector.push(parseInt(attribute));
            }
        });
        dataInstances.push(instance);
    });
    readCount++;
};

var experimentResults = {};
/** read the data file, get all wav file titles */
readResults = function (data) {
    var instanceArray = data.split("\n");
    // first instance contain the feature names
    var combinations = instanceArray[0].split("\t");
    combinations.forEach(function (value, i) {
        var el = {};
        experimentResults[value.trim() + ""] = instanceArray[1].split("\t")[i];
    })
};

app.post('/group', function (req, res) {
    var labelsFromSimulation = req.body.labelsFromSimulation;
    R(__dirname + "/group.R")
        .data(labelsFromSimulation)
        .call(function (err, d) {
            if (err) throw err;
            res.send({group: d})
        });
});

app.post('/probabilities', function (req, res) {
    var trainingData = req.body.trainingData;
    var group = req.body.group;
    var dataAll = trainingData.concat(mapDataExisting[req.session.user][group]);
    R(__dirname + "/model.R")
        .data(dataAll)
        .call(function (err, d) {
            if (err) throw err;
            res.send({probabilities: d, instanceOrder: instanceOrder})
        });
});

app.post('/dataInstances', function (req, res) {
    res.send({dataInstances: dataInstances, bilingual: req.session.bilingual})
});


app.post('/writeAnnotations', function (req, res) {
    var annotations = req.body.annotations;
    var group = req.body.group;
    var annotationsForGroup = annotations[group];
    updateDataInstanceLabels(annotationsForGroup);
    annotationsForGroup.forEach(function (annotation) {
        annotation.userID = req.session.user;
    });
    writeLabel(annotationsForGroup, group);
    res.send({done: true});
});

var updateDataInstanceLabels = function (annotationsForGroup) {
    var ids = [];
    var map = {};
    annotationsForGroup.forEach(function (d) {
        ids.push(d.stimuliID);
        map[d.stimuliID] = d;
    });
    dataInstances.forEach(function (d) {
        if (ids.indexOf(d.id) > -1) {
            //d.labeled = true;
            d.prediction_probability = map[d.id].prediction_probability;
        }
    });
};

var getGroup = function (labelsFromSimulation) {
    var group = R(__dirname + "/group.R")
        .data(labelsFromSimulation)
        .call(function (err, d) {
            if (err) throw err;
            console.log(d);
        });
    return group;
};

var instanceOrder;
var getInstanceOrder = function () {
    instanceOrder = R(__dirname + "/grid768.R")
        .callSync();
};

var read = function () {
    fs.readFile(__dirname + '/data/data_frame_new.txt', function (e, data) {
        if (e) throw e;
        readData(data.toString());
    });
};

app.get("/intro", (req, res) => {
    req.session.save(err => {
        if (err) {
            console.log(err);
        } else {
            if (readCount === 0) {
                read();
                getInstanceOrder();
            }
            res.render("intro");
        }
    });
});

app.post("/check", (req, res) => {
    var answers = req.body;
    answers.userID = req.session.user;
    answers.browser = req.session.browser;
    answers.os = req.session.os;
    if (answers.otherLanguages === "Ja") {
        req.session.bilingual = true;
    } else {
        req.session.bilingual = false;
    }
    writeParticipantInfo([answers]);
    res.render("check");
});

app.get("/questionnaire", (req, res) => {
    res.render("questionnaire");
});

app.get("/explanation", (req, res) => {
    res.render("explanation");
});

app.get("/task", (req, res) => {
    res.render("task");
});

app.get("/test-round", (req, res) => {
    res.render("test-round");
});

app.get("/test-stimuli", (req, res) => {
    res.render("test-stimuli");
});

app.get("/check2", (req, res) => {
    res.render("check2");
});

app.get("/task2", (req, res) => {
    res.render("task2");
});

app.get("/experiment", (req, res) => {
    res.render("experiment");
});

app.get("/audio", (req, res) => {
    setTimeout(function () {
        res.render("play", {src: "audio.wav"});
    }, 500);
});

app.get("/finish", (req, res) => {
    if (err) {
        console.log(err);
    } else {
        res.render("finish");
    }
});

app.get("/stop", (req, res) => {
    res.render("stop");
});

var writeLabel = function (annotation, group) {
    // write to mongodb
};

var writeParticipantInfo = function (answer) {
    // write to mongodb
};