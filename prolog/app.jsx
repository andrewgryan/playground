import React, { useState } from "react"
import ReactDOM from "react-dom"


const App = () => {
    const [ board, setBoard ] = useState(new Array(9).fill("b"))
    return (<div class="grid grid-cols-3">{
        board.map((content, i) => {
            const onClick = () => {
                const url = `/tictactoe?move=${i}&board=${board.join('')}`
                fetch(url)
                    .then(resp => resp.json())
                    .then(({ board }) => {
                        if (typeof board !== "undefined") {
                            setBoard(board)
                        }
                    })
            }
            let mark
            if (content === "b") {
                mark = ""
            } else {
                mark = content
            }
            return <Tile key={ i } onClick={ onClick }>{ mark }</Tile>
        })
        }</div>)
}


const Tile = ({ children, onClick }) => {
    return <div onClick={ onClick } class="h-6 w-6 bg-white border flex flex-row justify-center items-center">{ children }</div>
}


ReactDOM.render(<App />, document.getElementById("react-app"))
