import React, { useState } from "react"
import ReactDOM from "react-dom"


const App = () => {
    const blank = "b"
    const [ board, setBoard ] = useState(new Array(9).fill(blank))
    const [ winner, setWinner ] = useState(blank)
    return (
    <div class="bg-white rounded p-2 flex flex-col space-y-2">
        { winner !== blank && <div class="flex flex-row justify-center items-baseline text-thin">Winner <span class="pl-2 text-xl font-semibold">{ winner }</span></div> }
        <div class="grid grid-cols-3">
            { board.map((content, i) => {
                const onClick = () => {
                    const url = `/tictactoe?move=${i}&board=${board.join('')}`
                    fetch(url)
                        .then(resp => resp.json())
                        .then(({ board, winner }) => {
                            if (typeof board !== "undefined") {
                                setBoard(board)
                            }
                            setWinner(winner)
                        })
                }
                let mark
                if (content === blank) {
                    mark = ""
                } else {
                    mark = content
                }
                return <Tile key={ i } i={ i } onClick={ onClick }>{ mark }</Tile>
            })
            }
        </div>
    </div>)
}


const Tile = ({ children, onClick, i }) => {
    const classNames = ["h-8 w-8 font-mono text-gray-800 bg-white border-gray-900 flex flex-row justify-center items-center"]
    if ([6, 7, 8].indexOf(i) === -1) {
        classNames.push("border-b")
    }
    if ([2, 5, 8].indexOf(i) === -1) {
        classNames.push("border-r")
    }
    return <div onClick={ onClick } className={ classNames.join(" ") }>{ children }</div>
}


ReactDOM.render(<App />, document.getElementById("react-app"))
