
/* Experiment initializer.
 All the parameters for the experiment are defined here */

export class StimuliSettings {

  constructor({   sRandom,
                  nStim,
                  stimPath,
                  probs,
                  rewards,
                  losses
              } = {})

              {
                // Set-up and initialize experiment parameters
                this.sRandom = sRandom ;

                this.nStim = nStim;
                this.StimPath = stimPath;

                this.p = probs;
                this.r = rewards;
                this.l  = losses;

                // Initialize images
                this._loadImg(stimPath, nStim);

                // Create Schedule for presenting the images
                this._schedule()
              }

_loadImg() {
  this.StimImage=[];
    for (let i = 0; i <= this.nStim-1; i++){
      this.StimImage[i] = new Image();
      this.StimImage[i].src = this.StimPath+i+'.gif';}
}

_schedule() {
  // randomise values to symbols (i.e. which symbol will be 1 etc.)
  this.vi = [...Array(this.nStim).keys()];
  shuffle(this.vi);

  //  STIM - SCHEDULE
  this.A = [...Array(this.nStim).keys()];
  this.B = [...Array(this.nStim).keys()];

  this.Ac = [];
  this.Bc = [];

// Create a array (split into 2 by option) where options shown at the same time have the same value
  if (this.sRandom == 0){
    for (let i = 0; i< this.nStim;i=i+2){
        this.Ac.push(this.A[this.vi.indexOf(i)]);
        this.Bc.push(this.B[this.vi.indexOf(i+1)]);
    }
  }

  // Create a array (split into 2 by option) with all possible combinations the nStim stimuli (excluding 2 same stimuli)
  else if (this.sRandom == 1){
    for (let i = 0; i<= this.nStim-2;i++){
      for (let j = i+1;j<=this.nStim-1;j++){
        this.Ac.push(this.A[i]);
        this.Bc.push(this.B[j]);
      }
    }
  }

  // create a random schedule for displaying those
  this.si = [...Array(this.Ac.length).keys()];
  shuffle(this.si)

  // helper - Randomise on which side will be option A (iside[0]) be presented
  this.iside = [...Array(2).keys()];

  // helper - determines if reward (itop[0]==0) or probability (itop[0]==1) will be presented first
  this.itop = [...Array(2).keys()];
}

symbolSelector(selector,stim){
  return (this.vi[this[selector+"c"][this.si[stim]]])

}

}

function shuffle(array) {
  let counter = array.length;

  /* While there are elements in the array */
  while (counter > 0) {
    /* Pick a random index */
    let index = Math.floor(Math.random() * counter);

    /* Decrease counter by 1 */
    counter--;

    /* And swap the last element with it */
    let temp = array[counter];
    array[counter] = array[index];
    array[index] = temp; }}
