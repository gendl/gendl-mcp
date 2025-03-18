/** @type {import('tailwindcss').Config} */
module.exports = {
    content: ['../**/*.gdl' , '../**/*.gendl', '../**/*.lisp'],
    theme: {
        extend: {},
    },
    plugins: [
        require('@tailwindcss/forms'),
    ],
}
