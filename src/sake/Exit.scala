package sake {
    object Exit {
        /**
         * Error out of the build.
         */
        def error(message:String) = throw new BuildError(message)
    }
}