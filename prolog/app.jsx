import React from "react"
import ReactDOM from "react-dom"


const App = () => {
    const board = new Array(9).fill("x")
    return (<div class="grid grid-cols-3">{
        board.map((content, i) => {
            const onClick = () => {
                alert(i)
            }
            return <Tile key={ i } onClick={ onClick }>{ content }</Tile>
        })
        }</div>)
}


const Tile = ({ children, onClick }) => {
    return <div onClick={ onClick } class="h-6 w-6 bg-white border flex flex-row justify-center items-center">{ children }</div>
}


ReactDOM.render(<App />, document.getElementById("react-app"))
