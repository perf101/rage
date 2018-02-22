// Include this at the end of body, so DOM is loaded


const allRows = Array.from(document.querySelector('table').querySelectorAll('tr'));

// First 3 rows are fixed text (product, titles, comparison) - all the rest are data
const rows = Array.from(allRows);
rows.splice(0,3);

function getBaselineColumn(){
  // Baseline is a heading in table row 1
  tds = allRows[1].getElementsByTagName('td');
  baseline = -1;
  for(var i=0; i<tds.length; i++){
    if(tds[i].textContent == "Baseline"){
      baseline = i;
      break;
    }
  }
  return baseline;
}

// Only rows after baseline column are interesting
const baselineColumn = getBaselineColumn();

// Helper methods for filter code
const normalise = s =>
  // Normalise a cell value like (-7%) to 7, like (3%) to 3
  parseInt(s.replace('(', '').replace('%)', '').replace('-', '').replace('+', ''), 10);

const isCellEmpty = cell => 
  // If empty then the text content starts with a - followed by a space (as opposed to a negative number)
  cell.textContent.trim().startsWith('- ');

const getLastCellWithData = (row) => {
  tds = row.getElementsByTagName('td');
  for(var i=tds.length-1; i>baselineColumn; i--){
    if(!isCellEmpty(tds[i]))
      return tds[i];
  }
  return null
}

const isCellGreen = cell =>
  // Check a non-empty cell
  cell.querySelector('div span').style.color == 'green';

const showAllRows = () => rows.map(row => row.style.display = '');

const getSelectValue = (s) => s.options[s.selectedIndex].value;

function hideNonInterestingRows(regressionsOnly=true, threshold=5){
  for(var i=0; i<rows.length; i++){
    const row = rows[i];

    // Assess the last cell with data
    const lastDataCell = getLastCellWithData(row);    

    // Filter if no cell has data (TODO: maybe this should be a filter option?)
    if(lastDataCell === null){
      row.style.display = 'none';
      continue;
    }

    // Filter non-regressions if required
    if(regressionsOnly && isCellGreen(lastDataCell)){
      row.style.display = 'none';
      continue
    }

    const subs = lastDataCell.querySelector('div').querySelectorAll('sub');
    // Filter if no % in the last cell
    if(subs.length == 1){
      row.style.display = 'none';
      continue;
    }

    // second sub is the %
    const normalisedPercentage = normalise(subs[1].innerHTML);
    console.log(normalisedPercentage, threshold);
    if(normalisedPercentage < threshold){
      console.log('keeping');
      row.style.display = 'none'; 
      continue;
    }
  }
}

// DOM elements for filter UI
const filterEnabled = document.querySelector('input[name=filterEnabled]');
const filterType = document.querySelector('select[name=filterType]');
const minRegression = document.querySelector('input[name=minRegression]');

// DOM event for filtering (e is event, unused)
const filter = (e) => {
  if(!filterEnabled.checked){
    showAllRows();
    return;
  }

  const regressionsOnly = getSelectValue(filterType) === "regressions";  

  const threshold = parseInt(minRegression.value);
  showAllRows(rows);
  hideNonInterestingRows(regressionsOnly, threshold);
};

filterEnabled.onchange = filter;
filterType.onchange = filter;
minRegression.oninput = filter;

// Run the filter on page load too
filter({});
