import {StimuliSettings} from "./dStimuliSettings.js";
import {taskFixedFeedbackInterleaved} from "../FixedFeedbackInterleaved/TaskFixedFeedbackInterleaved.js";

import {sendToDB} from "../../general/dExperimentSendToDB.js";
import {orderFunc} from "../../general/order.js";
import {shuffle,getLastValue} from "../../general/dRandomFunctions.js";

var task1 = taskFixedFeedbackInterleaved.stimuliSettings;

var ss = new StimuliSettings(
    { nStim: task1.nStim,
      stimPath: task1.StimPath,

      probs: task1.p,
      rewards: task1.r,
      losses: task1.l,

      vi: task1.vi,
      A: task1.A,

      nResp: 5,
      RespPath: "images/values/",
      resps: [-90,-10,0,10,90],
      })

var taskValuations = {
      stimuliSettings:ss ,

      taskSettings:{
        taskName: 'Valuations',
        maxTrials: 24,//8,
        borderMS:500,//10,
      },

      results: {
        trial: 0, // number of trials
        stim: 0, //  between 0 - max trials in a block

        symbol: [],//  which symbol was presented
        symbolReward: [],
        symbolProb: [],

        optionList: [],  // in which order were the responses presented

        rewardChosen: [], // chosen reward
        rewardCorrect:[],  // correct reward
        respCorrect: [], // was the chosen response correct?

        respKey: [], // which response matrix was chosen (0-1)
        respKeyID: [], // ID of the pressed item - also includes buttons, and other clickable objects

        rt: [],  // reaction times
        rt_point: [],  // actual timepoints of each click

        tRew: 0, // total reward
      },

      init: function(test){
    	  let object = this;

        let Title = '<H3 align = "center">Task<br></H3>';

        let sym = '<div class="col" style="margin-top: 20px; margin-bottom: 20px;"><div align = "center"><canvas class="rounded" id="mySymbol" width="600" height="600" style="width: 100%;height: auto; max-width:200px;"></canvas></div></div>';
        let resp0 ='<div class="col"><div align = "center"><canvas class="rounded" id="myResp0" width="302" height="302" style="width: 100%;height: auto; max-width:120px;"></canvas></div></div>';
        let resp1 ='<div class="col"><div align = "center"><canvas class="rounded" id="myResp1" width="302" height="302" style=" width: 100%;height: auto; max-width:120px;"></canvas></div></div>';
        let resp2 ='<div class="col"><div align = "center"><canvas class="rounded" id="myResp2" width="302" height="302" style="width: 100%;height: auto; max-width:120px;"></canvas></div></div>';
        let resp3 ='<div class="col"><div align = "center"><canvas class="rounded" id="myResp3" width="302" height="302" style="width: 100%;height: auto; max-width:120x;"></canvas></div></div>';
        let resp4 ='<div class="col"><div align = "center"><canvas class="rounded" id="myResp4" width="302" height="302" style="width: 100%;height: auto; max-width:120px;"></canvas></div></div>';

        $('#Top').html(Title);
        $('#Stage').html(sym);
        $('#Vals').html(resp0+resp1+resp2+resp3+resp4);

        showStimuli(object,test);
      }
  };

export{taskValuations}



function  showStimuli(object,test){
    let rs = object.results;
    let ss = object.stimuliSettings;
    let ts = object.taskSettings;

    // if the trial number exceeded maximum number of trials - stop
    if(rs.trial==ts.maxTrials){endTask(object,test); return}

    //if all symbols have been shown, show them again
    if(rs.stim > ss.nStim-1){shuffle(ss.gi);  rs.stim =0 ;}

    // record the time at click
    rs.rt_point.push(Date.now());

    // show new STIMULI
    shuffle(ss.ri);

    drawMat(ss.StimImage[ss.gi[ss.A[rs.stim]]], "mySymbol",600);
    drawMat2(ss.RespImage[ss.ri[0]], "myResp0",302);
    drawMat2(ss.RespImage[ss.ri[1]], "myResp1",302);
    drawMat2(ss.RespImage[ss.ri[2]], "myResp2",302);
    drawMat2(ss.RespImage[ss.ri[3]], "myResp3",302);
    drawMat2(ss.RespImage[ss.ri[4]], "myResp4",302);

    // enable clicking
    document.getElementById("myResp0").onclick = function(){recordResponse(object,test)};
    document.getElementById("myResp1").onclick = function(){recordResponse(object,test)};
    document.getElementById("myResp2").onclick = function(){recordResponse(object,test)};
    document.getElementById("myResp3").onclick = function(){recordResponse(object,test)};
    document.getElementById("myResp4").onclick = function(){recordResponse(object,test)};
  }

  // record responses
function  recordResponse(object,test){
    let rs = object.results;
    let ss = object.stimuliSettings;
    let ts = object.taskSettings;


    let symbol = ss.vi[ss.gi[ss.A[rs.stim]]];

    // Prevent another button press
      document.getElementById("myResp0").onclick = "";
      document.getElementById("myResp1").onclick = "";
      document.getElementById("myResp2").onclick = "";
      document.getElementById("myResp3").onclick = "";
      document.getElementById("myResp4").onclick = "";

      // Record RT
      rs.rt.push(Date.now()-rs.rt_point[rs.rt_point.length-1]);

    // Record response and send to DB
      rs.respKey.push(parseInt(event.target.id.charAt(6))); // which position was chosen
      rs.respKeyID.push(event.target.id); // exact id of the position

      rs.symbol.push(ss.gi[ss.A[rs.stim]]);  // which symbol was presented
      rs.symbolReward.push(ss.r[symbol]);
      rs.symbolProb.push(ss.p[symbol]);

      rs.optionList.push([ss.ri]);  // in which order were the responses presented

      rs.rewardChosen.push(ss.resps[ss.ri[getLastValue(rs.respKey)]]) // chosen reward

      // was it the correct option?
      // if the symbols prob was < 50, correct answer is always 0
      if(getLastValue(rs.symbolProb)<50){
        rs.rewardCorrect.push(0); // correct reward
        rs.respCorrect.push(getLastValue(rs.rewardChosen)==0? 1 : 0);} //
      // otherwise check if chosen value matched the value of the option
      else {
        rs.rewardCorrect.push(getLastValue(rs.symbolReward)); // correct reward
        rs.respCorrect.push(getLastValue(rs.rewardCorrect)==getLastValue(rs.rewardChosen)? 1 : 0); }

      rs.tRew = rs.tRew + (getLastValue(rs.respCorrect)==1? 10:0);

   //Send to Database
   sendToDB(0,
   { partID: test.ID,
     expID: test.expID,
     tsName: ts.taskName,
     trial: rs.trial,
     tsBorderTime: ts.borderMS,
     reactionTime: getLastValue(rs.rt),
     respKey: getLastValue(rs.respKey),
     respKeyID: getLastValue(rs.respKeyID),
     symbol: getLastValue(rs.symbol),
     symbolID: symbol,
     symbolProb: getLastValue(rs.symbolProb),
     symbolReward:getLastValue(rs.symbolReward),
     rewardChosen:getLastValue(rs.rewardChosen),
     rewardCorrect:getLastValue(rs.rewardCorrect),
     respCorrect:getLastValue(rs.respCorrect),
     totalReward:rs.tRew,
   },
   'php/InsertValuationDataDB.php');

// highlight chosen option
 GV_highlight(object,test);
}

 function  GV_highlight(object,test) {
     let rs = object.results;
     let ss = object.stimuliSettings;
     let ts = object.taskSettings;

     // Highlight the chosen option
      let  frx = document.getElementById(event.target.id).getContext("2d");
        frx.lineWidth = "25";
        frx.strokeStyle = "black";
        frx.strokeRect(0, 0, 302, 302);

    // After X ms hide the border feedback and start a new trial - as long as this trial wasn't the last
      let newTrial =  setTimeout(function(){showStimuli(object,test)}, ts.borderMS);

    // start a new trial
       rs.trial ++
       rs.stim ++
     }

function     endTask(object,test){
       let ts = object.taskSettings;
       let rs = object.results;

       // clear everything
         $('#Stage').empty();
         $('#Vals').empty();
         $('#Bottom').empty();
         $('#Top').empty();


       // Write on the matrix end of the trial
         let Title = '<h3 align = "center" style="color: #3C455C;"> End </h3>'
         let EndText ='<div class="col"><h5 align = "center"> <br> This is the end of the task!</h5>  ' +
         '<p align = "center" ><br> You won '+rs.tRew+' points. <br></p></div>' ;

         let EndBut = '<div align="center"><input type="button"  class="btn btn-default" id="bEnd" value="Please click here to continue" style="background-color: #3C455C; color:#FFFFFF"></div>';

         $('#Top').html('<div class="row justify-content-center">'+Title+'</div>');
         $('#Stage').html('<div class="row justify-content-center">'+EndText+'</div>');
         $('#GameButton').html('<div class="row justify-content-center">'+EndBut+'</div>');

         document.getElementById("bEnd").onclick = function(){

                 $('#Top').empty();
                 $('#Stage').empty();
                 $('#Vals').empty();
                 $('#GameButton').empty();
                 $('#Bottom').empty();
                 orderFunc(test);
               };

     }

     function drawMat(image,id,size) {
       let Ax = document.getElementById(id).getContext("2d");
       Ax.drawImage(image,0,0);
       Ax.lineWidth = "5";
       Ax.strokeStyle = "black";
       Ax.strokeRect(0, 0, size, size);
     }

     function drawMat2(image,id,size) {
       let Ax = document.getElementById(id).getContext("2d");
       Ax.drawImage(image,0,0);
       Ax.lineWidth = "10";
       Ax.strokeStyle = "black";
       Ax.strokeRect(0, 0, size, size);
     }


     function drawResp(resp,id,size) {
       let Ax = document.getElementById(id).getContext("2d");
       Ax.fillText(resp,0,0);
       Ax.lineWidth = "5";
       Ax.strokeStyle = "black";
       Ax.strokeRect(0, 0, size, size);
     }
