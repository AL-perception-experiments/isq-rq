/****** PIPELINE *************************************************************************/
var user_group_instance_size = 24 + 4;// 24+4 number of instances to determine user group
var max_instance_size = 64 + 28;
var trainingData = [];
var labelsFromSimulation = [];
var trainingInstances = [];

// two group training datasets
var group0TrainingData = [];
var group1TrainingData = [];
var trainingDataForUserGroups = [group0TrainingData, group1TrainingData];
var dataInstanceMap = {};
var dataInstanceMapAllCues = {};
var annotations = [[], []];

var crossTimeout = 500;
var playTimeout = 1000;
var cueNames = ["q_type", "vq", "dur", "prenuc", "edge_tone", "nuc"];

var previousItem = "";
var pattern = "NONE";
var observedItems = [];

var catchItemCorrect = 0;
var catchItems = ["catchitem1_paraden_RQ.wav", "catchitem2_insekten_RQ.wav", "catchitem3_spaghetti_ISQ.wav", "catchitem4_trompete_ISQ.wav"];
var playedCatchItemsCount = 0;

var count = 0;

var dataInstances = [];
var bilingual = true;
var group = "NONE";

/*******************************************************************************************/

var getNextInstanceForUserGrouping = function (dataInstances) {
    var copy = [];
    dataInstances.forEach(function (d) {
        if (!d.labeled) {
            copy.push(d);
        }
    });
    shuffle(copy);
    var instance = copy[0];
    if (pattern === "NONE") {
        var cues = [];
        copy[0].cue_vector.forEach(function (c, i) {
            if (i < 4) {
                cues.push(c);
            }
        });
        previousItem = copy[0].item;
        pattern = cues.join("-");
        observedItems.push(copy[0].cue_vector[4] + "-" + copy[0].cue_vector[5]);
    } else {
        instance = getNotYetVisited(pattern);
    }
    return instance;
};

var getNextInstance = function (dataInstances) {
    var copy = [];
    dataInstances.forEach(function (d) {
        if (!d.labeled) {
            copy.push(d);
        }
    });
    shuffle(copy);
    copy.sort(function (a, b) {
        return Math.abs(a.prediction_probability - 0.5) - Math.abs(b.prediction_probability - 0.5);
    });
    return getDifferentItemToPrevious(copy);
};

var getDifferentItemToPrevious = function (copy) {
    var found;
    for (let i = 0; i < copy.length; i++) {
        var d = copy[i];
        if (d.item !== previousItem) {
            previousItem = d.item;
            found = d;
            break;
        } else {
            continue;
        }
    }
    if (found === undefined) found = copy[0];
    return found;
};

var specifyLabel = function (nextInstance, label, timeTaken) {
    var input = [];
    nextInstance.cue_vector.forEach(function (d) {
        input.push(d);
    });
    input.push(nextInstance.id);
    nextInstance.label = label;
    nextInstance.labeled = true;
    nextInstance.timeTaken = timeTaken;

    return label;
};

var getNotYetVisited = function (pattern) {
    var instances = dataInstanceMap[pattern];
    shuffle(instances);
    var instance = null;
    for (let i = 0; i < instances.length; i++) {
        var d = instances[i];
        var item = d.cue_vector[4] + "-" + d.cue_vector[5];
        if (observedItems.indexOf(item) === -1) {
            if (previousItem !== d.item) {
                instance = d;
                observedItems.push(item);
                break;
            }
        } else {
            continue;
        }
    }
    previousItem = instance.item;
    return instance;
};

function shuffle(array) {
    var counter = array.length;
    // While there are elements in the array
    while (counter > 0) {
        // Pick a random index
        var index = Math.floor(Math.random() * counter);
        // Decrease counter by 1
        counter--;
        // And swap the last element with it
        var temp = array[counter];
        array[counter] = array[index];
        array[index] = temp;
    }
    return array;
}

var createDataInstanceMap = function (dataInstances, map, mapAllCues) {
    dataInstances.forEach(function (d) {
        var cues = [];
        d.cue_vector.forEach(function (c, i) {
            if (i < 4) {
                cues.push(c);
            }
        });
        var key = cues.join("-");
        if (map[key] === undefined) {
            map[key] = [d];
        } else {
            map[key].push(d);
        }
    });

    dataInstances.forEach(function (d) {
        var cues = [];
        d.cue_vector.forEach(function (c) {
            cues.push(c);
        });
        var key = cues.join("-");
        if (mapAllCues[key] === undefined) {
            mapAllCues[key] = [d];
        } else {
            mapAllCues[key].push(d);
        }
    });
};

var addToTrainingData = function (nextInstance) {
    trainingData.push(createTrainingInstance(nextInstance));
};

var createTrainingInstance = function (nextInstance) {
    var trainingInstance = [];
    nextInstance.cue_vector.forEach(function (d) {
        trainingInstance.push(d);
    });
    trainingInstance.push(nextInstance.label);
    return trainingInstance;
};

var addToGroupTrainingData = function (group, d) {
    trainingDataForUserGroups[group] = trainingDataForUserGroups[group].concat(d);
};

var writeInstance = function (instance, group, index, grouping) {
    var annotation = {};
    instance.cue_vector.forEach(function (d, i) {
        annotation[cueNames[i]] = d;
    });
    annotation.label = instance.label;
    annotation.timeTaken = instance.timeTaken;
    annotation.stimuliID = instance.id;
    annotation.order = index;
    annotation.grouping = grouping;
    annotation.prediction_probability = instance.prediction_probability;
    annotations[group].push(annotation);
};

var updatePredictions = function (trainingData, dataInstances) {
    var xhr = new XMLHttpRequest();
    xhr.open("POST", "/probabilities", true);
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.send(JSON.stringify({
        trainingData: trainingData,
        group: group
    }));
    xhr.onload = function () {
        var probabilities = JSON.parse(this.responseText).probabilities;
        var instanceOrder = JSON.parse(this.responseText).instanceOrder;
        instanceOrder.forEach(function (d, j) {
            var i = 0;
            var cues = [];
            for (var key in d) {
                cues.push(d[key]);
                i++;
            }
            var instances = dataInstanceMapAllCues[cues.join("-")];
            instances.forEach(function (inst) {
                var id = inst.id;
                for (var k = 0; k < dataInstances.length; k++) {
                    var d = dataInstances[k];
                    if (d.id === id) {
                        d.prediction_probability = probabilities[j];
                        break;
                    }
                }
            });
            addNewProbabilities(cues.join("-"), probabilities[j]);
        });
    };
};

//update the predictions for the remaining instances afterward, only for a sufficient user
var updatePredictionsMainPipe = function (trainingData, dataInstances) {
    var xhr = new XMLHttpRequest();
    xhr.open("POST", "/probabilities", true);
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.send(JSON.stringify({
        trainingData: trainingData,
        group: group
    }));
    xhr.onload = function () {
        var probabilities = JSON.parse(this.responseText).probabilities;
        var instanceOrder = JSON.parse(this.responseText).instanceOrder;
        instanceOrder.forEach(function (d, j) {
            var i = 0;
            var cues = [];
            for (var key in d) {
                cues.push(d[key]);
                i++;
            }

            var instances = dataInstanceMapAllCues[cues.join("-")];
            instances.forEach(function (inst) {
                var id = inst.id;
                for (var k = 0; k < dataInstances.length; k++) {
                    var d = dataInstances[k];
                    if (d.id === id) {
                        d.prediction_probability = probabilities[j];
                        break;
                    }
                }
            });
            addNewProbabilities(cues.join("-"), probabilities[j]);
        });

        writeInstance(nextInstance, group, count, false);
        if (count < max_instance_size) {
            expPipe();
        } else {
            //console.log("pipeline write annotations");
            writeAnnotations();
        }
    };
};

var getTrainingDataForUser = function (userGroup) {
    return trainingDataForUserGroups[userGroup];
};

var t0;
var play = function (instance) {
    setTimeout(function () {
        audioIsPlayed = true;
        document.getElementById("audio-experiment").src = "stimuli/" + instance.filename_with_extension;
        document.getElementById("audio-experiment").play();
    }, playTimeout);
};

var audioIsPlayed = false;
var warningTimeout;
document.getElementById("audio-experiment").onended = function () {
    audioIsPlayed = false;
    t0 = performance.now();
    warningTimeout = setTimeout(function () {
        document.getElementById("warning").style.display = "block";
    }, 10000);
};


var user_input = function (e) {
    var label = "";
    var timeTaken = 0;
    e = e || window.event;
    var k = e.keyCode || e.which;
    var catchItemJustPlayed = false;
    if (count === 5 || count === 13 || count === 18 || count == 26) {
        catchItemJustPlayed = true;
    }
    if (k == 74 && !audioIsPlayed) {
        document.getElementById("warning").style.display = "none";
        clearTimeout(warningTimeout);
        audioIsPlayed = true;// allow only one click
        //catch item
        if (catchItemJustPlayed) {
            count++;
            label = 1;// RQ label
            if (count === 6 || count === 14) {
                catchItemCorrect++;
            }
            document.getElementById("progress").value = count;
            document.getElementById("cross").style.visibility = "visible";
            setTimeout(function () {
                document.getElementById("cross").style.visibility = "hidden";
            }, crossTimeout);
        } else {
            count++;
            if (count <= max_instance_size) {
                document.getElementById("progress").value = count;
                label = 1;// RQ label
                timeTaken = getTimeTaken();
                document.getElementById("cross").style.visibility = "visible";
                setTimeout(function () {
                    document.getElementById("cross").style.visibility = "hidden";
                }, crossTimeout);
            }
        }
    } else if (k == 70 && !audioIsPlayed) {
        document.getElementById("warning").style.display = "none";
        clearTimeout(warningTimeout);
        audioIsPlayed = true;//allow only one click
        //catch item
        if (catchItemJustPlayed) {
            count++;
            label = 0;// ISQ label
            if (count === 19 || count === 27) {
                catchItemCorrect++;
            }
            document.getElementById("progress").value = count;
            document.getElementById("cross").style.visibility = "visible";
            setTimeout(function () {
                document.getElementById("cross").style.visibility = "hidden";
            }, crossTimeout);
        } else {
            count++;
            if (count <= max_instance_size) {
                document.getElementById("progress").value = count;
                label = 0;// ISQ label
                timeTaken = getTimeTaken();
                document.getElementById("cross").style.visibility = "visible";
                setTimeout(function () {
                    document.getElementById("cross").style.visibility = "hidden";
                }, crossTimeout);
            }
        }
    }

    if (count <= user_group_instance_size) {
        //phase to determine user group
        if (label === 0 || label === 1) {
            if (!catchItemJustPlayed) {
                specifyLabel(nextInstance, label, timeTaken);
                // add the instance to the training data
                addToTrainingData(nextInstance);
                trainingInstances.push(nextInstance);
                // add the label to simulation labels to determine user group
                labelsFromSimulation.push(nextInstance.label);
            }
            //only if we there is less than 24 labels for user group
            if (count < user_group_instance_size) {
                if (count === 5 || count === 13 || count === 18 || count == 26) {
                    catchItem();
                } else {
                    userGroupPipe();
                }
            } else {
                //console.log("is valid user " + checkIfValidParticipant());
                checkIfValidParticipant();
                if (user_ok) {
                    if (group === "NONE") {
                        getGroup();
                        pauseExperiment();
                    }
                }
            }
        }
    } else {
        if (user_ok) {
            if (count <= max_instance_size) {
                if (label === 0 || label === 1) {
                    // specify label
                    specifyLabel(nextInstance, label, timeTaken);
                    //writeInstance(nextInstance, group);
                    addToGroupTrainingData(group, createTrainingInstance(nextInstance));
                    // update all predicted labels and probabilities
                    updatePredictionsMainPipe(getTrainingDataForUser(group), dataInstances);
                }
            }
        }
    }
};

var continued = false;

//timer 3 minuten!
function startTimer(duration, display) {
    var timer = duration, minutes, seconds;
    setInterval(function () {
        minutes = parseInt(timer / 60, 10);
        seconds = parseInt(timer % 60, 10);

        minutes = minutes < 10 ? "0" + minutes : minutes;
        seconds = seconds < 10 ? "0" + seconds : seconds;

        display.textContent = minutes + ":" + seconds;

        if (--timer < 0 && !continued) {
            window.open("url_for_finishing_experiment", "_self");
        }
    }, 1000);
}

var showTimer = function () {
    var threeMinutes = 60 * 3,
        display = document.querySelector('#time');
    startTimer(threeMinutes, display);
};

var pauseExperiment = function () {
    audioIsPlayed = true;
    document.getElementById("forAudio").style.display = "none";
    document.getElementById("forPause").style.display = "block";
    showTimer();
};

var checkIfValidParticipant = function () {
    user_ok = true;
    if (bilingual) {
        user_ok = false;
    }
    if (catchItemCorrect < 3) {
        user_ok = false;
    }

    return user_ok;
};

var getTimeTaken = function () {
    var t1 = performance.now();
    //console.log(t1-t0)
    return t1 - t0;
};

var writeAnnotations = function () {
    var xhr = new XMLHttpRequest();
    xhr.open("POST", "/writeAnnotations", true);
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.send(JSON.stringify({
        annotations: annotations,
        group: group
    }));
};

document.getElementById("body").onkeydown = user_input;

var nextInstance;
var userGroupPipe = function () {
    // --- Data: 24 questions --
    nextInstance = getNextInstanceForUserGrouping(dataInstances);
    // set the label
    play(nextInstance);
};


var catchItem = function () {
    playCatchItem(catchItems[playedCatchItemsCount]);
    playedCatchItemsCount++;
};

document.getElementById("warning").style.display = "none";
var playCatchItem = function (catchItem) {
    setTimeout(function () {
        //t0 = performance.now();
        document.getElementById("audio-experiment").src = "catch-items/" + catchItem;
        document.getElementById("audio-experiment").play();
    }, playTimeout);
};


var getGroup = function () {
    var xhr = new XMLHttpRequest();
    xhr.open("POST", "/group", true);
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.send(JSON.stringify({
        labelsFromSimulation: labelsFromSimulation
    }));
    xhr.onload = function () {
        group = parseInt(JSON.parse(this.responseText).group);
        // first, add the data that was annotated to determine the user group to the training data for the particular user group
        trainingData.forEach(function (d) {
            addToGroupTrainingData(group, d);
        });

        trainingInstances.forEach(function (d, i) {
            writeInstance(d, group, i, true);
        });

        updatePredictions(getTrainingDataForUser(group), dataInstances);
    };
};


var expPipe = function () {
    nextInstance = getNextInstance(dataInstances);
    play(nextInstance);
};


var mainPipeline = function () {
    var xhr = new XMLHttpRequest();
    xhr.open("POST", "/dataInstances", true);
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.send();
    xhr.onload = function () {
        dataInstances = JSON.parse(this.responseText).dataInstances;
        bilingual = JSON.parse(this.responseText).bilingual;
        createDataInstanceMap(dataInstances, dataInstanceMap, dataInstanceMapAllCues);
        document.getElementById("cross").style.visibility = "visible";
        setTimeout(function () {
            document.getElementById("cross").style.visibility = "hidden";
            userGroupPipe();
        }, crossTimeout);
    };
};

document.getElementById("cross").style.visibility = "hidden";

function startPlay() {
    document.getElementById("cross").style.visibility = "visible";
    document.getElementById("beginButton").style.display = "none";
    mainPipeline();
};

var allProbabilityMap = {};
var addNewProbabilities = function (cueComb, prob) {
    if (allProbabilityMap[cueComb] === undefined) {
        allProbabilityMap[cueComb] = [];
    }
    allProbabilityMap[cueComb].push(prob);
};


