(#let cv(author: "", contacts: (), body) = {
	show heading: it => [
    #pad(bottom: -10pt, [#smallcaps(it.body)])
    #line(length: 100%, stroke: 1pt)
  ]

  // Author
  align(center)[
    #block(text(weight: 700, 1.75em, author))
  ]

  // Contact information.
  pad(
    top: 0.5em,
    bottom: 0.5em,
    x: 2em,
    align(center)[
      #grid(
        columns: 4,
        gutter: 1em,
        ..contacts
      )
    ],
  )

  // Main body.
  set par(justify: true)

  body
}

#let icon(name, baseline: 1.5pt) = {
  box(
    baseline: baseline,
    height: 10pt,
    image(name)
  )
}

#let exp(place, title, location, time, details) = {
  pad(
    bottom: 10%,
    grid(
      columns: (auto, 1fr),
      align(left)[
        *#place* \
        #emph[#title]
      ],
      align(right)[
        #location \
        #time
      ]
    )
  )
  details
}

#import "resume.typ": *

#show: cv.with(
  author: "Wu Yu Wei",
  contacts: (
    [#icon("mail.svg") #link("mailto:yuweiwu@pm.me")],
    [#icon("home.svg") #link("https://wusyong.github.io/")[wusyong.github.io]],
    [#icon("github.svg") #link("https://github.com/wusyong")[Github]],
    [#icon("linkedin.svg") #link("https://www.linkedin.com/in/yu-wei-wu-23630a155/")[LinkedIn]],
  )
)

= Brief
A software developer interested in various programming domains. Core memeber of Tauri cross-platform framework. Proficient in Rust and C, but comfortable with most general programming languages.

= Education
#exp(
  "National Chiao Tung University",
  "Bachelor in Computer Science",
  "Taiwan",
  "08/2012 – 07/2016",
  []
)

= Experience
#exp(
  "CrabNebula Ltd.",
  "Director of Engineering",
  "Malta",
  "10/2022 – Present",
  [
    - Directing large open-source project with people across the globe. Developed and maintained most well-known web browser libraries. Researched the boundry of web engines and pushing them forward.
  ]
)
#exp(
  "Semnet Ltd.",
  "(System Programming) Senior Software Engineer",
  "Ireland",
  "01/2021 – 08/2022",
  [
    - Focus on building foundation of all services across multiple platforms. Resolved all platform-specific issues while bringing each systems’ capability for the team to build features on top of them.
    - Researched toward possible goals and designed best solutions for the company to promote the products.
  ]
)
#exp(
  "Chuehfu Technology Ltd.",
  "(Distributed Storage Systems) Senior Software Engineer",
  "Taiwan",
  "08/2019 – 11/2020",
  [
    - Developed the protocol of main system service. Built several essential components around the distributed system like async actor-based runtime, MPMC channel, and a fully hand-made protocol implementation to communicate with Cassandra / ScyllaDB.
  ]
)
#exp(
  "BiiLabs Inc.",
  "(Cloud Services) Software Engineer",
  "Taiwan",
  "01/2018 – 08/2019",
  [
    - Improved computation bottleneck significantly with various approach like SSE/AVX SIMD acceleration and building embedded FPGA cluster with rabbitMQ.
  ]
)

= Personal Projects
#exp(
  "Tauri",
  "Core Member",
  "https://github.com/tauri-apps/tauri",
  "",
  [
    A framework that builds smaller, faster, and more secure desktop and mobile applications with a web frontend.
    - Compatibile with any front-end framework.
    - Supports cross-platform compilation to bundle binaries for major desktop platforms.
  ]
)

= Skills
- *Programming Language*: *multilingual* (not limited to any specific language), especially experienced in Rust C, comfortable with C++ Haskell C\# Objective-C Python Swift Racket (in random order).
- *Rust*: familiar with Cargo and Bazel, understand procedural macros, Rust asynchronous runtimes, and unsafe usage, contributed to Rust compiler and its tools from time to time.
- *System Programming*: familiar with Linux programming interface and also other Unix-like OSes. Understand general architecture of the operating system and know how to write one from #link("https://youtu.be/vbZU7ABrAiE")[scratch].

= Miscellaneous
- Languages: English - fluent, Chinese - native, Taiwanese - native.
- Member of `Tauri`, `rust-tw`, and more, contributed to `rust`, `async-std`, `veloren`, `tauri`, `riscv-opcodes`, `grcov`, `windows-rs`, `gtk-rs`, `winit`, and other projects.
