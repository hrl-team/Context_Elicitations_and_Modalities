import {shuffle} from "../../general/dRandomFunctions.js";
/* Experiment initializer.
 All the parameters for the experiment are defined here */

export class StimuliSettings {

  constructor({   nStim,
                  stimPath,
                  probs,
                  rewards,
                  losses,
                  vi,
                  A,
                  nResp,
                  RespPath,
                  resps
              } = {})

              {
                // Set-up and initialize experiment parameters
                this.nStim = nStim;
                this.StimPath = stimPath;

                this.p = probs;
                this.r = rewards;
                this.l  = losses;

                this.vi = vi;
                this.A = A;

                this.nResp = nResp;
                this.RespPath = RespPath;
                this.resps = resps

                // Initialize images
                this._loadStimImg(stimPath, nStim);
                this._loadRespImg(stimPath, nStim);

                // Create Schedule for presenting the images
                this._schedule()
              }

_loadStimImg() {
  this.StimImage=[];
    for (let i = 0; i <= this.nStim-1; i++){
      this.StimImage[i] = new Image();
      this.StimImage[i].src = this.StimPath+i+'.gif';}
}

_loadRespImg() {
  this.RespImage=[];
  for (let i = 0; i <= this.nResp-1; i++){
    this.RespImage[i] = new Image();
    this.RespImage[i].src = this.RespPath+'cf_'+this.resps[i]+'.jpg';}
  }

_schedule() {
  // randomise order of presented symbols
  this.gi = [...Array(this.nStim).keys()];

  // helper randomise sides for guess value task
  this.ri = [...Array(this.nResp).keys()];
}
}
