import {StimuliSettings} from "./dStimuliSettings.js";
import {taskFixedFeedbackInterleaved} from "../FixedFeedbackInterleaved/TaskFixedFeedbackInterleaved.js";

import {sendToDB} from "../../general/dExperimentSendToDB.js";
import {orderFunc} from "../../general/order.js";
import {shuffle,getLastValue} from "../../general/dRandomFunctions.js";


var ss = new StimuliSettings(
    { sRandom:0,

      nStim: 4,
      stimPath: "images/stimTrain/",

      probs: [75,25,75,25] ,
      rewards: [-10,-10,10,10],
      losses: [0,0,0,0]
    })

var trainingFixedFeedback = {
          stimuliSettings:ss ,
          taskSettings:{
              taskName: 'TrainingTask',

              sFeedback:1,
              sVals:0,
              sFreeChoice:0,
	            sButtonChoice: 1,

              maxTrials: 16,//12,
      	      blockTrials: 1,
              maxRepCount: 2, //maximum repetitions of the task

              fdbMS:2500,
              borderMS:500,
              valsMS:0
          },

          results: {
            trial: 0, // number of trials
            block: 0,  // between 0 - max unique stimuli combinations shown in the trial
            stim: 0, //  between 0 - max trials in a block
            repCount: 0, // how many times was the task be repeated

            sym_ch: [], // chosen symbol
            p_ch: [],  // probability
            r_ch: [], // reward of the chosen option if lottery wins
            l_ch: [], // loss of the chosen option - if loses
            out_ch: [], // outcome of the chosen option

            sym_un: [],// which symbol was presented as option B
            p_un: [],  // probability
            r_un: [],  // reward of option B - if wins
            l_un: [], // reward of the option B - if loses
            out_un: [],

            respKey: [], // which response matrix was chosen (0-1)
            respKeyID: [], // ID of the pressed item - also includes buttons, and other clickable objects

            rt: [],  // reaction times
            rt_point: [],  // actual timepoints of each click

            tRew: 0, // total reward
        },

        init: function(test){
	         let object = this;
	         let ts = object.taskSettings;

          let Title = '<H3 align = "center">Task<br></H3>';
          $('#Top').html(Title);

          let resp0 ='<div class="col"><div align = "center"><canvas class="border rounded" id="myResp0" width="600" height="600" style="width: 100%; height: auto; max-width:200px;"></canvas></div></div>';
          let resp1 ='<div class="col"><div align = "center"><canvas class="border rounded" id="myResp1" width="600" height="600" style="width: 100%; height: auto; max-width:200px;"></canvas></div></div>';
          $('#Stage').html(resp0+resp1);

          if(ts.sFreeChoice == 1 ){
            var buttonN ='<div align="col m-5"><input align="center" type="button"  class="btn btn-default rounded m-2 invisible" id="bNext" value="Next" style="background-color: #3C455C; color:#FFFFFF"></div>';
            $('#GameButton').html(buttonN);
          }

          showStimuli(object,test);
        }
    };


export{trainingFixedFeedback}

function  showStimuli(object,test){
    let ts = object.taskSettings;
    let rs = object.results;
    let ss = object.stimuliSettings;

    document.getElementById("ContBox").className = "col-12 mt-3 visible";

    // if the trial number exceeded maximum number of trials - stop
    if(rs.trial==(ts.maxTrials)){endTask(object,test); return}

    rs.rt_point.push(Date.now()) // records the actual time at click

    //if all trials in a block were shown, start a different block
    if(rs.stim == ts.blockTrials){rs.block++;  rs.stim =0 ;}

    //if all blocks were shown, but the max trials were not reach, reshufle the order and start again
    if(rs.block > ss.Ac.length-1){shuffle(ss.si);  rs.block =0 ;}

    // Show new stimuli
    shuffle(ss.iside);
    drawStim(object,"A",0,rs.block);
    drawStim(object,"B",1,rs.block);

    if(ts.sFreeChoice == 1){
        document.getElementById("bNext").className = "btn btn-default m-2 rounded  visible";
    }

    //show new stimuli  -values (and hide them after fdbMS sec)

    let rewardSymbolA = ss.r[ss.symbolSelector("A",rs.block) ];
    let rewardSymbolB = ss.r[ss.symbolSelector("B",rs.block) ];

    let probSymbolA = ss.p[ss.symbolSelector("A",rs.block) ];
    let probSymbolB = ss.p[ss.symbolSelector("B",rs.block) ];


    shuffle(ss.itop)

    if (ts.sVals==1){
      if (ss.itop[0]==0){
        var vals1 = '<div class="col"><h4 align = "center" style="font-size: min(1.5em, 16px);">'+rewardSymbolA+
                                        'p <br>'+probSymbolA+'% </h4></div>';

        var vals2 = '<div class="col"><h4 align = "center" style="font-size: min(1.5em, 16px);">'+rewardSymbolB+
                                      'p <br>'+probSymbolB+'% </h4></div>';
      }
      else if (ss.itop[0]==1){
        var vals1 = '<div class="col"><h4 align = "center" style="font-size: min(1.5em, 16px);">'+probSymbolA+
                                        '% <br>'+rewardSymbolA+'p </h4></div>';

        var vals2 = '<div class="col"><h4 align = "center" style="font-size: min(1.5em, 16px);">'+probSymbolB+
                                      '% <br>'+rewardSymbolB+'p </h4></div>';
      }

      if (ss.iside[0]==0){
        $('#Vals').html(vals1+vals2);
      }
      else {
        $('#Vals').html(vals2+vals1);
      }

      // what to do next - if free choice - hide values by clicking a button, otherwise they will be automatically hidden
      if (ts.sFreeChoice == 1){
        document.getElementById("bNext").onclick = function(){hideInfo(object,test)} ;
      }
      else {
        setTimeout(function(){hideInfo(object,test)}, ts.valsMS);
      }

    }

    else if(ts.sVals==0&& ts.sFreeChoice == 0 && ts.sButtonChoice == 0){
      document.getElementById("myResp0").onclick = function(){recordResponse(object,test)};
      document.getElementById("myResp1").onclick = function(){recordResponse(object,test)};
    }

    else if(ts.sVals==0&& ts.sFreeChoice == 0 && ts.sButtonChoice == 1){
	    var buttonL ='<input align="center" type="button"  class="btn btn-default btn-block rounded mt-3 mb-3 visible bLeft" id="myButL0" >';
      var buttonR ='<input align="center" type="button"  class="btn btn-default btn-block rounded mt-3 mb-3 visible bRight" id="myButR1" ">';

        if (Math.round(Math.random())==0){
            $('#GameButton').html('<div align="col m-1">'+buttonL+buttonR+'</div>');
        }else{
            $('#GameButton').html('<div align="col m-1">'+buttonR+buttonL+'</div>');
        }

        document.getElementById("myButL0").onclick = function(){recordResponse(object,test)};
        document.getElementById("myButR1").onclick = function(){recordResponse(object,test)};
      }


    else if(ts.sVals==0&& ts.sFreeChoice == 1){
      document.getElementById("bNext").onclick = function(){hideInfo(object,test)} ;
    }

  }

function   recordResponse(object,test) {
      let ts = object.taskSettings;
      let rs = object.results;
      let ss = object.stimuliSettings;

      let symbolA = ss.symbolSelector("A",rs.block) // equals toss.vi[ss.Ac[ss.si[ss.stim]]]
      let symbolB = ss.symbolSelector("B",rs.block) // equals to ss.vi[ss.Bc[ss.si[ss.stim]]]

    // Prevent another button press
      document.getElementById("myResp0").onclick = "";
      document.getElementById("myResp1").onclick = "";

    if(ts.sButtonChoice == 1 ){
      //  document.getElementById("myButL0").className = "btn btn-default m-2 rounded btn-block invisible";
      //  document.getElementById("myButR1").className = "btn btn-default m-2 rounded btn-block invisible";

      $('#GameButton').empty();

        }

   // Record RT
    if (ts.sVals==0){
      rs.rt.push(Date.now()-rs.rt_point[rs.rt_point.length-1]);
    } else if (ts.sVals==1){
      rs.rt.push(Date.now()-rs.rt_point[rs.rt_point.length-1]-ts.valsMS);
    }

    // Record response
      rs.respKey.push(parseInt(event.target.id.charAt(6)));
      rs.respKeyID.push(event.target.id);

      let chRespKey = parseInt(event.target.id.charAt(6));

    // Record positioning
    // If the A option was chosen (the same response button as position of option A)
      rs.sym_ch.push((chRespKey == ss.iside[0] ? ss.Ac[ss.si[rs.block]] : ss.Bc[ss.si[rs.block]]));
      rs.p_ch.push((chRespKey == ss.iside[0] ? ss.p[symbolA] : ss.p[symbolB]));
      rs.r_ch.push((chRespKey == ss.iside[0] ? ss.r[symbolA] : ss.r[symbolB]));
      rs.l_ch.push((chRespKey == ss.iside[0] ? ss.l[symbolA] : ss.l[symbolB]));

      rs.sym_un.push((chRespKey == ss.iside[1] ? ss.Ac[ss.si[rs.block]] : ss.Bc[ss.si[rs.block]]));
      rs.p_un.push((chRespKey == ss.iside[1] ? ss.p[symbolA] : ss.p[symbolB]));
      rs.r_un.push((chRespKey == ss.iside[1] ? ss.r[symbolA] : ss.r[symbolB]));
      rs.l_un.push((chRespKey == ss.iside[1] ? ss.l[symbolA] : ss.l[symbolB]));

    // Test for the probability condition, if true add R-value of the chosen option, if not add l-value
      let ran_ch = Math.floor(Math.random()*100)
      rs.out_ch.push(ran_ch > getLastValue(rs.p_ch) ? getLastValue(rs.l_ch) : getLastValue(rs.r_ch))
      rs.tRew = rs.tRew + getLastValue(rs.out_ch)    // record reward so far

      let ran_un = Math.floor(Math.random()*100) //math.randomInt(1, 100)
      rs.out_un.push(ran_un > getLastValue(rs.p_un) ? getLastValue(rs.l_un) : getLastValue(rs.r_un))

    // SendtoDB(rs.trial)
      sendToDB(0,
      { partID: test.ID,
        expID: test.expID,
        tsName: ts.taskName,
  taskOrder: test.order == 0? "PreferenceFirst": "ValuationFirst",
        trial: rs.trial,
        stim: rs.stim,
        block: rs.block,
        repCount: rs.repCount,
        tsFeedback: ts.sFeedback,
        tsVals: ts.sVals,
        tsChoice: ts.sFreeChoice,
        tsButtons: ts.sButtonChoice,
        tsFeedbackTime:ts.fdbMS,
        tsValsTime: ts.valsMS,
        tsBorderTime: ts.borderMS,
        tsRandom: ss.sRandom,
        reactionTime: getLastValue(rs.rt),
        respKey: getLastValue(rs.respKey),
        respKeyID: getLastValue(rs.respKeyID),
        symbolChosen: getLastValue(rs.sym_ch),
        probChosen: getLastValue(rs.p_ch),
        rewardChosen:getLastValue(rs.r_ch),
        lossChosen:getLastValue(rs.l_ch),
        randomChosen:ran_ch,
        outcomeChosen:getLastValue(rs.out_ch),
        symbolUnchosen:getLastValue(rs.sym_un),
        probUnchosen: getLastValue(rs.p_un),
        rewardUnchosen:getLastValue(rs.r_un),
        lossUnchosen:getLastValue(rs.l_un),
        randomUnchosen:ran_un,
        outcomeUnchosen:getLastValue(rs.out_un),
        totalReward:rs.tRew,
	choiceType:ss.si[rs.block]
      },
      'php/InsertTrainingDataDB.php'
  );

    // Highlight chosen option
    if(ts.sButtonChoice == 0 ){highlightOption(event.target.id)}
    else if(ts.sButtonChoice == 1 ){highlightOption("myResp"+getLastValue(rs.respKey))}


     // After 500ms hide the border feedback and start a new trial - as long as this trial wasn't the last

     if (ts.sFeedback==0){
      // newTrial =  setTimeout("NewTrial2()", borderMS);
       var newTrial =  setTimeout(function(){fix(object,test)}, ts.borderMS);
       rs.trial ++
       rs.stim ++
     }

     else if (ts.sFeedback==1){
       var newFeed =  setTimeout(function(){feedback(object,test)}, ts.borderMS);
       rs.trial ++
       rs.stim ++
     }
  }

function   drawStim(object,symbol,isd,stim) {

    let rs = object.results;
    let ss = object.stimuliSettings;
    let opt = object.stimuliSettings[symbol+"c"]

    let Ax = document.getElementById("myResp"+ss.iside[isd]).getContext("2d")
    Ax.drawImage(ss.StimImage[opt[ss.si[stim]]],0,0);
  }

function   hideInfo(object,test) {
    let ts = object.taskSettings;

    $('#Vals').empty()

    // enable clicking again - when clicked reponse will be saved and feedback shown
    if(ts.sFreeChoice == 1){
        document.getElementById("bNext").className = "btn btn-default m-2 rounded  invisible";
    }

    if(ts.sButtonChoice == 1 ){
         var buttonL ='<input align="center" type="button"  class="btn btn-default btn-block rounded mt-3 mb-3 visible bLeft" id="myButL0" >'; //btn-block m-2
      	 var buttonR ='<input align="center" type="button"  class="btn btn-default btn-block rounded mt-3 mb-3 visible bRight" id="myButR1" >'; //btn-block  m-2


      if (Math.round(Math.random())==0){
          $('#GameButton').html('<div align="col"><div class="btn-group-vertical mt-2 mb-2">'+buttonL+buttonR+'</div></div>');
      }

      else{
          $('#GameButton').html('<div align="col"><div class="btn-group-vertical mt-2 mb-2">'+buttonR+buttonL+'</div></div>');
      }

      document.getElementById("myButL0").onclick = function(){recordResponse(object,test)};
      document.getElementById("myButR1").onclick = function(){recordResponse(object,test)};

    }

  else {
      document.getElementById("myResp0").onclick = function(){recordResponse(object,test)};
      document.getElementById("myResp1").onclick = function(){recordResponse(object,test)};
    }
  }

function   highlightOption(optionId) {
    let frx = document.getElementById(optionId).getContext("2d");
    frx.lineWidth = "25";
    frx.strokeStyle = "black";
    frx.strokeRect(0, 0, 600, 600);
  }

function   feedback(object,test){
    let ts = object.taskSettings;
    let rs = object.results;

    let fdb_ch = '<div class="col"><H4 align = "center">'+getLastValue(rs.out_ch)+'p</H4></div>';
    let fdb_un = '<div class="col"><H4 align = "center">'+getLastValue(rs.out_un)+'p</H4></div>';

    if (getLastValue(rs.respKey) == 0){ $('#Vals').html(fdb_ch+fdb_un); }
    else { $('#Vals').html(fdb_un+fdb_ch); }

    if(ts.sFreeChoice == 0 ){
      setTimeout(function(){hideFeedback(object,test)}, ts.fdbMS);
    }
    else{
      document.getElementById("bNext").onclick = function(){hideFeedback(object,test)};
      document.getElementById("bNext").className = "btn btn-default m-2 rounded  visible";
    }
  }

function   hideFeedback(object,test){
    let ts = object.taskSettings;
    $('#Vals').empty();
    if(ts.sFreeChoice == 1){
        document.getElementById("bNext").className = "btn btn-default m-2 rounded  invisible";
    }

    fix(object,test)
  }

function   fix(object,test){
    let ts = object.taskSettings;
    setTimeout(function(){showStimuli(object,test)}, 200);
    document.getElementById("ContBox").className = "col-12 mt-3 invisible";
    if(ts.sFreeChoice == 1){
        document.getElementById("bNext").className = "btn btn-default m-2 rounded  invisible";
    }

    }

function   endTask(object,test){
    let ts = object.taskSettings;
    let rs = object.results;

    // clear everything
    $('#Stage, #Vals, #Bottom, #Top').empty();


    // Write on the matrix end of the trial
      var Title = '<h3 align = "center" style="color: #3C455C;"> End </h3>';
      var EndText ='<div class="col"><br><br><h5 align = "center"> This is the end of training! </h5></div>';

      var buttonNext = '<input align="center" type="button"  class="btn btn-default m-2 myBtn" id="bNext" value="Start the task">';
      if (rs.repCount <=2){
          var buttonBack = '<input align="center" type="button"  class="btn btn-default m-2 myBtn" id="bBack" value="Repeat the training">';
        }

      else {
        var buttonBack = '<input align="center" type="button"  class="btn btn-default m-2 myBtn invisible" id="bBack" value="Repeat the training">';
      }

      $('#Top').html('<div class="row justify-content-center">'+Title+'</div>');
      $('#Stage').html('<div class="row justify-content-center">'+EndText+'</div>');
      $('#Bottom').html(buttonBack+buttonNext);

      document.getElementById("bNext").onclick = function(){

            $('#Stage, #Vals, #Bottom, #Top, #GameButton').empty();
            orderFunc(test);
      };

      document.getElementById("bBack").onclick = function(){
          rs.trial=0;
          rs.stim = 0;
          rs.block = 0;
          rs.tRew =0;
          rs.repCount ++;
          $('#Stage, #Vals, #Bottom, #Top , #GameButton').empty();
          object.init(test);
      };

  }
