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
    array[index] = temp;
  }
}


function getLastValue(myArray){
      return(myArray[myArray.length-1])
}

function findCor(index) {
      return index == 0;
    }

function disableF5(e) {
if ((e.which || e.keyCode) == 116) e.preventDefault();
}

function points2pounds(points,rate) {
  let numb = points*rate;
  numb = numb.toFixed(2);
  return numb
}

export {shuffle, getLastValue,findCor,disableF5,points2pounds}
