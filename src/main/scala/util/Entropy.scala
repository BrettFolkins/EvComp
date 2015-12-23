package com.util

import java.util.concurrent.ThreadLocalRandom

object Entropy {
    def rand = ThreadLocalRandom.current()
}
