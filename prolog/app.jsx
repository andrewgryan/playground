import React, { useState } from "react"
import ReactDOM from "react-dom"


const App = () => {
    const blank = "b"
    const [ board, setBoard ] = useState(new Array(9).fill(blank))
    const [ winner, setWinner ] = useState(blank)
    return (
    <div>
        { winner !== blank && <div>Winner: { winner }</div> }
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
                return <Tile key={ i } onClick={ onClick }>{ mark }</Tile>
            })
            }
        </div>
    </div>)
}


const Tile = ({ children, onClick }) => {
    return <div onClick={ onClick } class="h-6 w-6 bg-white border flex flex-row justify-center items-center">{ children }</div>
}


ReactDOM.render(<App />, document.getElementById("react-app"))
